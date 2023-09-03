package tdimhcsleumas.sat.solver.lib.algorithms

import scala.annotation.tailrec
import scala.math
import org.log4s._

class DPLLAlg extends AlgTrait {
    case class ProblemInstance(
        cnf: Seq[Seq[Int]],
        assignment: Seq[Int]
    )

    private[this] val logger = getLogger
    // https://en.wikipedia.org/wiki/DPLL_algorithm#The_algorithm

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

    def findPure(cnf: Seq[Seq[Int]]): Option[Int] = {
        val flattened = cnf.flatMap(clause => clause)

        val literalToDefinedSet = flattened.foldLeft[Map[Int, Int]](Map()) { (map, literal) =>
            val absLiteral = math.abs(literal)
            val definedSet = map.getOrElse(absLiteral, 0)
            val updatedSet = if (literal > 0) {
                definedSet | 2
            } else {
                definedSet | 1
            }
            map + ((absLiteral, updatedSet))
        }

        literalToDefinedSet
            .find { case (_, definedSet) =>
                definedSet == 2 || definedSet == 1
            }
            .map { case (i, definedSet) =>
                if (definedSet == 2) {
                    i
                } else {
                    -1 * i
                }
            }
    }


    @tailrec private def pureLiteralElimination(instance: ProblemInstance): Option[ProblemInstance] = {
        val maybePure = findPure(instance.cnf)
        maybePure match {
            case None => Some(instance)
            case Some(pure) => {
                logger.debug(s"Eliminating pure: $pure")

                val newCnf = propagate(instance.cnf, pure)
                if (newCnf.find(literals => literals.length == 0).isDefined) {
                    None
                } else {
                    val newAssignment = instance.assignment :+ pure
                    pureLiteralElimination(instance.copy(cnf = newCnf, assignment = newAssignment))
                }
            }
        }
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
        val flattened = cnf.flatMap(clause => clause)

        val numToCountMap = flattened.foldLeft[Map[Int, Int]](Map()) { (map, literal) =>
            val absLiteral = math.abs(literal)
            val count = map.getOrElse(absLiteral, 0)
            map + ((absLiteral, count + 1))
        }

        val (maxI, _) = numToCountMap.max
        maxI
    }


    def solveRecurse(instance: ProblemInstance): Option[Seq[Int]] = {
        logger.debug(s"assigned: ${instance.assignment.length} variables")

        // unit propagation
        val maybePropagation = unitPropagation(instance).flatMap { unitInstance =>
            pureLiteralElimination(unitInstance)
        }

        maybePropagation match {
            case None => None
            case Some(unitInstance) if unitInstance.cnf.length == 0 => Some(unitInstance.assignment)
            case Some(unitInstance) => {
                // backtracking
                val ProblemInstance(cnf, assignment) = unitInstance
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


    override def solve(cnf: Seq[Seq[Int]]): Option[Seq[Int]] = solveRecurse(ProblemInstance(cnf, Seq()))
}
