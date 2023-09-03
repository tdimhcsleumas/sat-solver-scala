package tdimhcsleumas.sat.solver.lib.algorithms

import scala.annotation.tailrec
import scala.math
import org.log4s._

import cats.syntax.all._

class ImplicationGraph(private val graph: Map[Int, Set[Int]] = Map()) {
    def implies(implVertices: Set[Int], vertex: Int): ImplicationGraph = {
        val newGraph = graph + ((vertex, implVertices))
        new ImplicationGraph(newGraph)
    }

    def findCut(vertexA: Int, vertexB: Int): Set[Int] = {
        val implA = graph.getOrElse(vertexA, Set[Int]())
        val implB = graph.getOrElse(vertexB, Set[Int]())

        implA.intersect(implB)
    }
}

/*
Differences from DPLL:
* maintain two copies of the problem: the original and the applied one.
    (with the goal being able to determine which assigned variables are implying the current unit)
* maintain a history of all the decisions
* 
*/

case class ProblemInstance(
    cnf: Seq[Seq[Int]],
    assignment: Seq[Int]
)

class CDCLAlg extends AlgTrait {
    private[this] val logger = getLogger

    def propagateUnit(cnf: Seq[Seq[Int]], unit: Int): Seq[Seq[Int]] = {
        cnf.flatMap { literals =>
            if (literals.contains(unit)) {
                List()
            } else {
                val inverseUnit = -1 * unit
                List(literals.filter(literal => literal != inverseUnit))
            }
        }
    }

    @tailrec private def propagateUnits(instance: ProblemInstance, units: List[Int]): Either[Int, ProblemInstance] = {
        units match {
            case Nil => Right(instance)
            case unit :: tail => {
                val updatedCnf = propagateUnit(instance.cnf, unit)
                if (updatedCnf.find(literals => literals.isEmpty).isDefined) {
                    Left(unit)
                } else {
                    propagateUnits(ProblemInstance(updatedCnf, instance.assignment :+ unit), tail)
                }
            }
        }
    }

    private def findUnits(cnf: Seq[Seq[Int]]): List[Int] = {
        cnf.filter(clause => clause.length == 1).map(_(0)).toList
    }

    private def chooseLiteral(cnf: Seq[Seq[Int]]): Int = {
        val flattened = cnf.flatMap(clause => clause)

        val numToCountMap = flattened.foldLeft[Map[Int, Int]](Map()) { (map, literal) =>
            val count = map.getOrElse(literal, 0)
            map + ((literal, count + 1))
        }

        val (maxI, _) = numToCountMap.max
        maxI
    }

    private def tryAssign(instance: ProblemInstance, literal: Int): Option[Seq[Int]] = {
        val maybePropagated = propagateUnits(instance, findUnits(instance.cnf :+ Seq(literal)))

        maybePropagated match {
            case Left(unit) => {
                logger.debug(s"$unit caused the conflict")
                None
            }
            case Right(ProblemInstance(cnf, assignment)) if (cnf.isEmpty) => Some(assignment)
            case Right(updatedInstance) => solveRecurse(updatedInstance)
        }
    }

    private def solveRecurse(instance: ProblemInstance): Option[Seq[Int]] = {
        val literal = chooseLiteral(instance.cnf)

        val withLiteral = tryAssign(instance, literal)
        withLiteral match {
            case None => tryAssign(instance, -1 * literal)
            case Some(_) => withLiteral
        }
    }

    override def solve(cnf: Seq[Seq[Int]]): Option[Seq[Int]] = solveRecurse(ProblemInstance(cnf, Seq()))
}