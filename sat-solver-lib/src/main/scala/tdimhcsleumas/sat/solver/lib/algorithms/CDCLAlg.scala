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
        cnf: Seq[Seq[(Int, Boolean)]],
        variables: Set[Int],
        assignment: Map[Int, Boolean]
    )

    private[this] val logger = getLogger

    case class History(
        history: Map[Set[Int], ProblemInstance]
    )

    def findUnit(instance: ProblemInstance): Option[(Int, Boolean)] = {
        val ProblemInstance(cnf, _, assignment) = instance
        val maybeUnit = cnf.find { clause =>
            // unit: 
            // a. 1 literal clause
            // b. other assigned literals are false in the clause
            val satisfiedVariable = clause.find { case(variable, isTrue) =>
                assignment.get(variable).map(asgn => asgn == isTrue).getOrElse(false)
            }
            if (satisfiedVariable.isDefined) {
                false
            } else {
                clause.filter { case(variable, _) => assignment.get(variable).isEmpty }.length == 1
            }
        }
        maybeUnit.flatMap(_.find { case(variable, _) => assignment.get(variable).isEmpty })
    }

    @tailrec final def unitPropagation(instance: ProblemInstance): Option[ProblemInstance] = {
        val maybeUnit = findUnit(instance)
        maybeUnit match {
            case None => Some(instance)
            case Some(unit) => {
                logger.debug(s"Eliminating unit: $unit")

                val newAssignment = instance.assignment + unit

                val conflictingClause = instance.cnf.find { clause =>
                    clause.filter { case (variable, isTrue) =>
                        newAssignment.get(variable) match {
                            case None => true
                            case Some(asgn) => asgn == isTrue
                        }
                    }.length == 0
                }

                if (conflictingClause.isDefined) {
                    None
                } else {
                    unitPropagation(instance.copy(assignment = newAssignment))
                }
            }
        }
    }

    def chooseLiteral(instance: ProblemInstance): Option[(Int, Boolean)] = {
        val unassigned = instance.variables.find(variable => !instance.assignment.contains(variable))
        unassigned.map(variable => (variable, true))
    }

    // consider only the first uip for back tracking
    def solveRecurse(instance: ProblemInstance): Option[Map[Int, Boolean]] = {
        logger.debug(s"assigned: ${instance.assignment.size} variables")


        // unit propagation
        val maybePropagation = unitPropagation(instance)

        maybePropagation match {
            case None => None
            case Some(unitInstance) => {
                val ProblemInstance(cnf, _, assignment) = unitInstance
                chooseLiteral(unitInstance) match {
                    case None => Some(assignment)
                    case Some(literal) => {
                        // backtracking
                        logger.debug(s"Guessing: $literal")

                        val maybeInclude = solveRecurse(unitInstance.copy(cnf = cnf :+ Seq(literal)))
                        maybeInclude match {
                            case None => {
                                val (variable, isTrue) = literal
                                val opposite = (variable, !isTrue)

                                logger.debug(s"Guessing: $opposite")

                                solveRecurse(unitInstance.copy(cnf = cnf :+ Seq(opposite)))
                            }
                            case _ => maybeInclude
                        }
                    }
                }
            }
        }
    }

    override def solve(cnf: Seq[Seq[Int]]): Option[Seq[Int]] = {
        val mappedCnf = cnf.map { clause => clause.map(num => (num.abs, num > 0)) }
        val variables = cnf.flatMap { clause => clause.map(_.abs) }.toSet
        val instance = ProblemInstance(mappedCnf, variables, Map())

        solveRecurse(instance).map { assignment =>
            assignment.map { case(variable, isTrue) =>
                if (isTrue) {
                    variable
                } else {
                    -1 * variable
                }
            }.toSeq
        }
    }
}