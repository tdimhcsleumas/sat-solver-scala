package tdimhcsleumas.sat.solver.lib.algorithms

import scala.annotation.tailrec
import scala.math
import org.log4s._

class DPLLAlg extends AlgTrait {
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

    @tailrec private def unitPropagation(
        cnf: Seq[Seq[Int]],
        assignment: Seq[Int]
    ): (Seq[Seq[Int]], Seq[Int]) = {
        val maybeUnit = findUnit(cnf)
        maybeUnit match {
            case None => (cnf, assignment)
            case Some(unit) => {
                logger.debug(s"Eliminating unit: $unit")

                val newCnf = propagate(cnf, unit)
                val newAssignment = assignment :+ unit
                unitPropagation(newCnf, newAssignment)
            }
        }
    }

    @tailrec private def pureLiteralElimination(
        cnf: Seq[Seq[Int]],
        assignment: Seq[Int]
    ): (Seq[Seq[Int]], Seq[Int]) = {
        val maybePure = findPure(cnf)
        maybePure match {
            case None => (cnf, assignment)
            case Some(pure) => {
                logger.debug(s"Eliminating pure: $pure")

                val newCnf = propagate(cnf, pure)
                val newAssignment = assignment :+ pure
                pureLiteralElimination(newCnf, newAssignment)
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

    def solveRecurse(
        cnf: Seq[Seq[Int]],
        assignment: Seq[Int]
    ): Option[Seq[Int]] = {
        logger.debug(s"assigned: ${assignment.length} variables")

        // unit propagation
        val (unitCnf, unitAssignment) = unitPropagation(cnf, assignment)

        // pure literal elimination
        val (pureCnf, pureAssignment) =
            pureLiteralElimination(unitCnf, unitAssignment)

        if (pureCnf.length == 0) {
            logger.debug("Succeeded!")

            Some(pureAssignment)
        } else if (pureCnf.find(clause => clause.isEmpty).isDefined) {
            logger.debug("Failed!")

            None
        } else {
            // backtracking
            val maxI = chooseLiteral(pureCnf)

            logger.debug(s"Guessing: $maxI")

            val maybeInclude = solveRecurse(pureCnf :+ Seq(maxI), pureAssignment)
            maybeInclude match {
                case None => {
                    logger.debug(s"Guessing: -$maxI")
                    solveRecurse(pureCnf :+ Seq(-1 * maxI), pureAssignment)
                }
                case _ => maybeInclude
            }
        }
    }

    override def solve(cnf: Seq[Seq[Int]]): Option[Seq[Int]] = solveRecurse(cnf, Seq())
}
