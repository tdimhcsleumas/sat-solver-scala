package tdimhcsleumas.sat.solver.lib.algorithms

import scala.annotation.tailrec
import scala.math
import org.log4s._

import cats.syntax.all._

class ImplicationGraph(private val graph: Map[Int, Set[Int]] = Map()) {
    private[this] val logger = getLogger

    def implies(implVertices: Set[Int], vertex: Int): ImplicationGraph = {
        val existingImp = graph.getOrElse(vertex, Set[Int]())
        val newGraph = graph + ((vertex, implVertices.union(existingImp)))
        new ImplicationGraph(newGraph)
    }

    private def materializeImplSet(vertex: Int): Set[Int] = {
        val maybeImpl = graph.get(vertex)
        maybeImpl match {
            case None => Set()
            case Some(impl) => impl.foldLeft(impl) { (set, v) =>
                set.union(materializeImplSet(v))
            }
        }
    }

    def findCut(vertexA: Int, vertexB: Int): Set[Int] = {
        // val implA = graph.getOrElse(vertexA, Set[Int]())
        // val implB = graph.getOrElse(vertexB, Set[Int]())
        val implA = materializeImplSet(vertexA)
        val implB = materializeImplSet(vertexB)

        logger.info(s"implA: ${implA} implies $vertexA")
        logger.info(s"implB: ${implB} implies $vertexB")

        implA.intersect(implB)
    }

    override def toString(): String = {
        graph.toString()
    }
}

/*
Differences from DPLL:
* maintain two copies of the problem: the original and the applied one.
    (with the goal being able to determine which assigned variables are implying the current unit)
* maintain a history of all the decisions
* 
*/


class CDCLAlg extends AlgTrait {
    case class ProblemInstance(
        origCnf: Seq[Seq[Int]],
        cnf: Seq[Seq[Int]],
        assignment: Seq[Int],
        implGraph: ImplicationGraph,
    )

    case class History(
        history: Map[Set[Int], ProblemInstance]
    )

    private[this] val logger = getLogger

    def propagate(cnf: Seq[Seq[Int]], unit: Int): Seq[Seq[Int]] = {
        cnf.flatMap { literals =>
            if (literals.contains(unit)) {
                List()
            } else {
                val inverseUnit = -1 * unit
                List(literals.filter(literal => literal != inverseUnit))
            }
        }
    }

    def findUnits(cnf: Seq[Seq[Int]]): Seq[Int] = {
        cnf.filter(clause => clause.length == 1).map(_(0))
    }

    def findUnit(cnf: Seq[Seq[Int]]): Option[Int] = {
        cnf.find(clause => clause.length == 1).map(_(0))
    }

    @tailrec private def unitPropagation(instance: ProblemInstance): Either[Set[Int], ProblemInstance] = {
        val maybeUnit = findUnit(instance.cnf)
        maybeUnit match {
            case None => Right(instance)
            case Some(unit) => {
                logger.debug(s"Eliminating unit: $unit")

                val newCnf = propagate(instance.cnf, unit)
                if (newCnf.find(literals => literals.length == 0).isDefined) {
                    logger.info(s"assignment: ${instance.assignment}")
                    instance.cnf.foreach { literals =>
                        logger.info(s"$literals")
                    }
                    logger.info(s"conflicts on $unit")
                    Left(instance.implGraph.findCut(unit, -1 * unit))
                } else {
                    val newAssignment = instance.assignment :+ unit

                    // if any *new* units were created as a result of this propagation, note the implication
                    val implGraph = findUnits(newCnf).foldLeft(instance.implGraph) { (graph, newUnit) =>
                        // find the assignments implicating this one
                        // ignore clauses where the assignment agrees
                        val implicators = instance.origCnf.filter { clause =>
                            clause.contains(newUnit) && clause.find(literal => newAssignment.contains(literal)).isEmpty
                        }.flatMap { clause =>
                            newAssignment.filter(literal => clause.contains(-1 * literal))
                        }.toSet

                        graph.implies(implicators, newUnit)
                    }

                    unitPropagation(instance.copy(cnf = newCnf, assignment = newAssignment, implGraph = implGraph))
                }
            }
        }
    }

    // this is likely the place to try out new heuristics
    // for now, select the literal with the highest occurence in the cnf
    def chooseLiteral(cnf: Seq[Seq[Int]]): Int = {
        val flattened = cnf.flatMap(clause => clause.map(_.abs)).toSet

        -1 * flattened.min
    }

    def solveRecurse(instance: ProblemInstance, history: Seq[ProblemInstance]): Either[Set[Int], Seq[Int]] = {
        logger.debug(s"assigned: ${instance.assignment.length} variables")

        // unit propagation
        val maybePropagation = unitPropagation(instance)

        maybePropagation match {
            case Left(cut) => {
                logger.info(s"cut: $cut")
                val additionalClause = cut.toSeq.map(num => -1 * num)

                history.zipWithIndex.foreach { case(prevInstance, i) =>
                    logger.info(s"previousInstance: $i")
                    logger.info("cnf")
                    prevInstance.cnf.foreach { clause =>
                        logger.info(s"$clause")
                    }
                    logger.info(s"assignment: ${prevInstance.assignment}")
                    logger.info("")
                }

                val maybeHistory = history.zipWithIndex
                    .find { case(prevInstance, _) => prevInstance.assignment.find(num => cut.contains(num)).isDefined }

                maybeHistory match {
                    case None => Left(cut)
                    case Some((_, i)) if i == 0 => Left(cut)
                    case Some((_, i)) => {
                        val selected = i - 1
                        val prevInstance = history(selected)
                        val updatedOrigCnf = prevInstance.origCnf :+ additionalClause
                        val updatedCnf = prevInstance.cnf :+ additionalClause

                        logger.info(s"$selected: ${prevInstance.assignment}")

                        Left(cut)
                    }
                } 
            }
            case Right(unitInstance) if unitInstance.cnf.length == 0 => Right(unitInstance.assignment)
            case Right(unitInstance) => {
                // backtracking
                val ProblemInstance(_, cnf, assignment, _) = unitInstance
                val literal = chooseLiteral(cnf)

                logger.info(s"Guessing: $literal")

                val positiveInstance = unitInstance.copy(cnf = cnf:+ Seq(literal), assignment = assignment)
                val maybeInclude = solveRecurse(positiveInstance, history :+ positiveInstance)
                maybeInclude match {
                    case Left(cut) => {
                        logger.info(s"Guessing: ${-1 * literal}")
                        val negativeInstance = unitInstance.copy(cnf = cnf :+ Seq(-1 * literal), assignment = assignment)
                        solveRecurse(negativeInstance, history :+ negativeInstance)
                    }
                    case _ => maybeInclude
                }
            }
        }
    }

    override def solve(cnf: Seq[Seq[Int]]): Option[Seq[Int]] = {
        val initialProblem = ProblemInstance(cnf, cnf, Seq(), new ImplicationGraph)
        solveRecurse(initialProblem, Seq(initialProblem)) match {
            case Left(_) => None
            case Right(assignment) => Some(assignment)
        }
    }
}