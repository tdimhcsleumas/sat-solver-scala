package tdimhcsleumas.sat.solver.lib.algorithms

import scala.annotation.tailrec
import scala.math
import org.log4s._

import cats.syntax.all._

class ImplicationGraph(private val graph: Map[Int, Set[Int]] = Map()) {
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
        val implA = graph.getOrElse(vertexA, Set[Int]())
        val implB = graph.getOrElse(vertexB, Set[Int]())
        // val implA = materializeImplSet(vertexA)
        // val implB = materializeImplSet(vertexB)

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

    def findUnit(cnf: Seq[Seq[Int]]): Option[Int] = {
        cnf.find(clause => clause.length == 1).map(_(0))
    }

    @tailrec private def unitPropagation(instance: ProblemInstance): Option[ProblemInstance] = {
        val maybeUnit = findUnit(instance.cnf)
        maybeUnit match {
            case None => Some(instance)
            case Some(unit) => {
                logger.debug(s"Eliminating unit: $unit")

                val newCnf = propagate(instance.cnf, unit)
                if (newCnf.find(literals => literals.length == 0).isDefined) {
                    None
                } else {
                    val newAssignment = instance.assignment :+ unit
                    unitPropagation(instance.copy(cnf = newCnf, assignment = newAssignment))
                }
            }
        }
    }

    // this is likely the place to try out new heuristics
    // for now, select the literal with the highest occurence in the cnf
    def chooseLiteral(cnf: Seq[Seq[Int]]): Int = {
        val flattened = cnf.flatMap(clause => clause.map(_.abs)).toSet

        flattened.min
    }

    def solveRecurse(instance: ProblemInstance): Option[Seq[Int]] = {
        logger.debug(s"assigned: ${instance.assignment.length} variables")

        // unit propagation
        val maybePropagation = unitPropagation(instance)

        maybePropagation match {
            case None => None
            case Some(unitInstance) if unitInstance.cnf.length == 0 => Some(unitInstance.assignment)
            case Some(unitInstance) => {
                // backtracking
                val ProblemInstance(_, cnf, assignment, _) = unitInstance
                val literal = chooseLiteral(cnf)

                logger.debug(s"Guessing: $literal")

                val maybeInclude = solveRecurse(unitInstance.copy(cnf = cnf:+ Seq(literal), assignment = assignment))
                maybeInclude match {
                    case None => {
                        logger.debug(s"Guessing: -$literal")
                        solveRecurse(unitInstance.copy(cnf = cnf :+ Seq(-1 * literal), assignment = assignment))
                    }
                    case _ => maybeInclude
                }
            }
        }
    }

    override def solve(cnf: Seq[Seq[Int]]): Option[Seq[Int]] = solveRecurse(ProblemInstance(cnf, cnf, Seq(), new ImplicationGraph))
}