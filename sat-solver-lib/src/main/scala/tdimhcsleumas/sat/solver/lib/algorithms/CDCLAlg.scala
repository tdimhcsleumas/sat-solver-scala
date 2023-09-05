package tdimhcsleumas.sat.solver.lib.algorithms

import scala.annotation.tailrec
import scala.math
import org.log4s._

import cats.syntax.all._


/*
Differences from DPLL:
* maintain two copies of the problem: the original and the applied one.
    (with the goal being able to determine which assigned variables are implying the current unit)
* maintain a history of all the decisions
* 
*/


class CDCLAlg extends AlgTrait {
    private[this] val logger = getLogger

    case class TrailEnt(
        literal: (Int, Boolean), // 0 for conflic symbol
        decisionLevel: Int,
        reason: Option[Seq[(Int, Boolean)]], // either a decision or a clause
    )

    case class ProblemInstance(
        cnf: Seq[Seq[(Int, Boolean)]],
        variables: Set[Int],
        assignment: Map[Int, Boolean],
        decisionLevel: Int,
        trail: Seq[TrailEnt],
    )

    private val conflictSymbol = (0, true)

    class ImplicationGraph[A](private val graph: Map[A, Set[A]] = Map[A, Set[A]]()) {
        private[this] val logger = getLogger

        def implies(implVertices: Set[A], vertex: A): ImplicationGraph[A] = {
            val existingImp = graph.getOrElse(vertex, Set[A]())
            val newGraph = graph + ((vertex, implVertices.union(existingImp)))
            new ImplicationGraph(newGraph)
        }

        private def materializeImplSet(vertex: A): Set[A] = {
            val maybeImpl = graph.get(vertex)
            maybeImpl match {
                case None => Set()
                case Some(impl) => impl.foldLeft(impl) { (set, v) =>
                    set.union(materializeImplSet(v))
                }
            }
        }

        def findCut(vertexA: A): Set[A] = {
            ???
        }

        override def toString(): String = {
            graph.toString()
        }
    }

    case class History(
        history: Map[Set[Int], ProblemInstance]
    )

    def findUnitClause(instance: ProblemInstance): Option[((Int, Boolean), Seq[(Int, Boolean)])] = {
        val ProblemInstance(cnf, _, assignment, _, _) = instance
        val maybeUnitClause = cnf.find { clause =>
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
        val maybeUnit = maybeUnitClause.flatMap(_.find { case(variable, _) => assignment.get(variable).isEmpty })
        (maybeUnit, maybeUnitClause).mapN((unit, clause) => (unit, clause))
    }

    def findConflict(instance: ProblemInstance): Option[Seq[(Int, Boolean)]] = {
        val conflictingClause = instance.cnf.find { clause =>
            clause.filter { case (variable, isTrue) =>
                instance.assignment.get(variable) match {
                    case None => true
                    case Some(asgn) => asgn == isTrue
                }
            }.length == 0
        }

        conflictingClause
    }

    @tailrec final def unitPropagation(instance: ProblemInstance): Either[Seq[TrailEnt], ProblemInstance] = {
        val maybeClause = findUnitClause(instance)
        maybeClause match {
            case None => Right(instance)
            case Some((unit, clause)) => {
                logger.debug(s"Eliminating unit: $unit")
  
                val newInstance = instance.copy(
                    assignment = instance.assignment + unit,
                    trail = instance.trail :+ TrailEnt(
                        unit,
                        instance.decisionLevel,
                        Some(clause)
                    )
                )

                val maybeConflictingClause = findConflict(newInstance)

                maybeConflictingClause match {
                    case None => unitPropagation(newInstance)
                    case Some(conflictClause) =>
                        Left(newInstance.trail :+ TrailEnt(conflictSymbol, instance.decisionLevel, Some(conflictClause)))
                }
            }
        }
    }

    def chooseLiteral(instance: ProblemInstance): Option[(Int, Boolean)] = {
        val unassigned = instance.variables.find(variable => !instance.assignment.contains(variable))
        unassigned.map(variable => (variable, true))
    }

    // precondition: non-empty trail 
    def conflictAnalysis(instance: ProblemInstance, trail: Seq[TrailEnt]): ProblemInstance = {
        // build the implication graph
        val implicationGraph = trail.foldLeft(new ImplicationGraph[(Int, Boolean)]) { (graph, entry) =>
            val TrailEnt(literal, _, reason) = entry
            val (variable, _) = literal
            reason match {
                case Some(clause) => {
                    val implicators = clause.filter{ case (testVariable, _) => testVariable != variable }
                        .map{ case (implVariable, isTrue) => (implVariable, !isTrue) }
                        .toSet
                    graph.implies(implicators, literal)
                }
                case None => graph.implies(Set(), literal)
            }
        }
        val literalDecisionLevels = trail.foldLeft(Map[(Int, Boolean), TrailEnt]()) { (map, entry) => map + ((entry.literal, entry)) }

        // find the cut
        val cut = implicationGraph.findCut(conflictSymbol)
        val learnedClause = cut.toSeq.map { case (variable, isTrue) => (variable, !isTrue) }
        val newCnf = instance.cnf :+ learnedClause

        if (cut.size == 1) {
            // return to level 0
            instance.copy(cnf = newCnf, assignment = Map(), decisionLevel = 0, trail = Seq())
        } else {
            // unsafely grab the second highest level
            val levels = learnedClause.map {literal => literalDecisionLevels.get(literal).get.decisionLevel }
                .sortWith(_ > _)
            val level = levels(1)

            // delete assignments in levels greater than level
            val updatedAssignments = instance.assignment.filter { literal =>
                literalDecisionLevels.get(literal).get.decisionLevel <= level
            }

            // delete trail items in levels greater than level
            val updatedTrail = trail.filter { case TrailEnt(_, decisionLevel, _) => decisionLevel <= level }

            instance.copy(cnf = newCnf, assignment = updatedAssignments, decisionLevel = level, trail = updatedTrail)
        }
    }

    // consider only the first uip for back tracking
    // in the trail of assignment, we need to differentiate between a decided assignment and a unit assignment
    def solveRecurse(instance: ProblemInstance): Either[Seq[TrailEnt], Map[Int, Boolean]] = {
        logger.debug(s"assigned: ${instance.assignment.size} variables")

        // unit propagation
        val maybePropagation = unitPropagation(instance)

        maybePropagation match {
            case Left(trail) => Left(trail)
            case Right(unitInstance) => {
                chooseLiteral(unitInstance) match {
                    case None => Right(unitInstance.assignment)
                    case Some(literal) => {
                        // backtracking
                        logger.debug(s"Guessing: $literal")

                        val decisionLevel = unitInstance.decisionLevel + 1

                        val decisionInstance = unitInstance.copy(
                            assignment = unitInstance.assignment + literal,
                            decisionLevel = decisionLevel,
                            trail = unitInstance.trail :+ TrailEnt(literal, decisionLevel, None)
                        )
                        val maybeInclude = solveRecurse(decisionInstance)
                        maybeInclude match {
                            case Left(trail) => solveRecurse(conflictAnalysis(decisionInstance, trail)) // conflict analysis. backtrack to appropriate decision level
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
        val instance = ProblemInstance(mappedCnf, variables, Map(), 0, Seq())

        solveRecurse(instance) match {
            case Left(_) => None
            case Right(assignment) => {
                val seqAssignment = assignment.map { case(variable, isTrue) =>
                    if (isTrue) {
                        variable
                    } else {
                        -1 * variable
                    }
                }.toSeq
                Some(seqAssignment)
            }
        }
    }
}