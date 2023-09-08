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
        reason: Option[Seq[(Int, Boolean)]] // either a decision or a clause
    )

    case class ProblemInstance(
        cnf: Seq[Seq[(Int, Boolean)]],
        variables: Set[Int],
        assignment: Map[Int, Boolean],
        decisionLevel: Int,
        trail: Seq[TrailEnt]
    )

    case class GraphNode[A](
        node: A,
        implies: Set[A],
        implicators: Set[A]
    )

    private val conflictSymbol = (0, true)

    class ImplicationGraph[A](private val graph: Map[A, GraphNode[A]] = Map[A, GraphNode[A]]()) {
        private[this] val logger = getLogger

        def implies(implVertices: Set[A], vertex: A): ImplicationGraph[A] = {
            val newImpl = graph.getOrElse(vertex, GraphNode(vertex, Set(), Set()))

            val appliedGraph = implVertices.foldLeft(graph) { (g, implVertex) =>
                val existingImpl = graph.getOrElse(implVertex, GraphNode(implVertex, Set(), Set()))

                g + ((implVertex, existingImpl.copy(implies = existingImpl.implies + vertex)))
            }

            val newGraph =
                appliedGraph + ((vertex, newImpl.copy(implicators = newImpl.implicators.union(implVertices))))
            new ImplicationGraph(newGraph)
        }

        private def enumeratePaths(start: A): Seq[Seq[A]] = {
            val vertex = graph.get(start).get

            if (vertex.implies.size == 0) {
                Seq(Seq(start))
            } else {
                val partialPaths = vertex.implies.toSeq.flatMap { vertex => enumeratePaths(vertex) }
                partialPaths.map { path => start +: path }
            }
        }

        def findUips(start: A): Set[A] = {
            val paths = enumeratePaths(start)

            paths.toList match {
                case head :: tail => {
                    tail.foldLeft(head.toSet) { (set, comp) => set.intersect(comp.toSet) }
                }
                case head :: Nil => head.toSet
                case Nil         => Set()
            }
        }

        def enumerateConflictSet(start: A): Set[A] = {
            val vertex = graph.get(start).get

            vertex.implies.foldLeft(Set[A]()) { (set, child) =>
                Set(child).union(set).union(enumerateConflictSet(child))
            }
        }

        // the conflict cut should be the partitioning of vertices where all decicion literal vertices
        // belong to set A, and the conflict vertex belong to set B.
        // the reason set include all vertices from set A with an edge in set B.

        // set B is the set of all predecessors of the cut vertex. Set A can be derived from the difference in the
        // set of all vertices. ie A = V - B. As such, iterate through A to find vertices that have an edge in B.
        def findCut(vertex: A): Set[A] = {
            val conflictSet = enumerateConflictSet(vertex)

            val decisionSet = graph.keySet.diff(conflictSet)

            decisionSet.filter { vert =>
                graph.get(vert).get.implies.find { implied => !decisionSet.contains(implied) }.isDefined
            }
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
            val satisfiedVariable = clause.find { case (variable, isTrue) =>
                assignment.get(variable).map(asgn => asgn == isTrue).getOrElse(false)
            }
            if (satisfiedVariable.isDefined) {
                false
            } else {
                clause.filter { case (variable, _) => assignment.get(variable).isEmpty }.length == 1
            }
        }
        val maybeUnit = maybeUnitClause.flatMap(_.find { case (variable, _) => assignment.get(variable).isEmpty })
        (maybeUnit, maybeUnitClause).mapN((unit, clause) => (unit, clause))
    }

    def findConflict(instance: ProblemInstance): Option[Seq[(Int, Boolean)]] = {
        val conflictingClause = instance.cnf.find { clause =>
            clause.filter { case (variable, isTrue) =>
                instance.assignment.get(variable) match {
                    case None       => true
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
                        Left(
                          newInstance.trail :+ TrailEnt(conflictSymbol, instance.decisionLevel, Some(conflictClause))
                        )
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
                    val implicators = clause
                        .filter { case (testVariable, _) => testVariable != variable }
                        .map { case (implVariable, isTrue) => (implVariable, !isTrue) }
                        .toSet
                    graph.implies(implicators, literal)
                }
                case None => graph.implies(Set(), literal)
            }
        }

        val literalDecisionLevels = trail.foldLeft(Map[(Int, Boolean), TrailEnt]()) { (map, entry) =>
            map + ((entry.literal, entry))
        }

        // find the cut

        val latestDecisionVariable = trail
            .filter { case TrailEnt(_, _, reason) => reason.isEmpty }
            .maxBy(_.decisionLevel)

        val uips = implicationGraph.findUips(latestDecisionVariable.literal) - conflictSymbol

        val firstCut = trail.reverse.find { case TrailEnt(literal, _, _) => uips.contains(literal) }.get

        val cut = implicationGraph.findCut(firstCut.literal)

        val learnedClause = cut.toSeq.map { case (variable, isTrue) => (variable, !isTrue) }

        val newCnf = instance.cnf :+ learnedClause

        if (cut.size == 1) {
            // return to level 0
            instance.copy(cnf = newCnf, assignment = Map(), decisionLevel = 0, trail = Seq())
        } else {
            // unsafely grab the second highest level
            val levels = learnedClause
                .map { case (variable, isTrue) => literalDecisionLevels.get((variable, !isTrue)).get.decisionLevel }
                .sortWith(_ > _)
            val level = levels(1)

            // delete trail items in levels greater than level
            val updatedTrail = trail.filter { case TrailEnt(_, decisionLevel, _) => decisionLevel <= level }

            // delete assignments in levels greater than level
            val updatedAssignments = updatedTrail.foldLeft(Map[Int, Boolean]()) { (map, ent) =>
                val TrailEnt(literal, _, _) = ent
                map + literal
            }

            instance.copy(cnf = newCnf, assignment = updatedAssignments, decisionLevel = level, trail = updatedTrail)
        }
    }

    // consider only the first uip for back tracking
    // in the trail of assignment, we need to differentiate between a decided assignment and a unit assignment
    @tailrec final def solveRecurse(instance: ProblemInstance): Option[Map[Int, Boolean]] = {
        logger.debug(s"assigned: ${instance.assignment.size} variables")

        // unit propagation
        val maybePropagation = unitPropagation(instance)

        maybePropagation match {
            case Left(trail) => {
                if (instance.decisionLevel == 0) {
                    None
                } else {
                    val backtrackInstance = conflictAnalysis(instance, trail)
                    solveRecurse(backtrackInstance) // conflict analysis. backtrack to appropriate decision level
                }
            }
            case Right(unitInstance) => {
                chooseLiteral(unitInstance) match {
                    case None => Some(unitInstance.assignment)
                    case Some(literal) => {
                        // backtracking

                        val decisionLevel = unitInstance.decisionLevel + 1

                        val decisionInstance = unitInstance.copy(
                          assignment = unitInstance.assignment + literal,
                          decisionLevel = decisionLevel,
                          trail = unitInstance.trail :+ TrailEnt(literal, decisionLevel, None)
                        )

                        solveRecurse(decisionInstance)
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
            case None => None
            case Some(assignment) => {
                val seqAssignment = assignment.map { case (variable, isTrue) =>
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
