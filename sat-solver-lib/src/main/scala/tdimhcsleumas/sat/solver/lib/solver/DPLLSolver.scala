package tdimhcsleumas.sat.solver.lib.solver

import scala.annotation.tailrec

case class Literal(i: Int, isDefined: Boolean)

case class Clause(literals: Seq[Literal])

class DPLLSolver {
    // https://en.wikipedia.org/wiki/DPLL_algorithm#The_algorithm
    

    def propagate(cnf: Seq[Clause], unit: Literal): Seq[Clause] = {
        cnf.flatMap { clause =>
            val Clause(literals) = clause
            val Literal(i, isDefined) = unit

            if (literals.contains(unit)) {
                List()
            } else {
                val inverseUnit = Literal(i, !isDefined)
                List(Clause(literals.filter(literal => literal != inverseUnit)))
            }
        }
    }

    def findUnit(cnf: Seq[Clause]): Option[Literal] = {
        cnf.find(clause => clause.literals.length == 1).map(_.literals(0))
    }

    def findPure(cnf: Seq[Clause]): Option[Literal] = {
        val flattened = cnf.flatMap(clause => clause.literals)

        val literalToDefinedSet = flattened.foldLeft[Map[Int, Int]](Map()) { (map, literal) =>
            val Literal(i, isDefined) = literal
            val definedSet = map.getOrElse(i, 0)
            val updatedSet = if (isDefined) {
                definedSet | (1 << 1)
            } else {
                definedSet | (1 << 0)
            }
            map + ((i, updatedSet))
        }

        literalToDefinedSet.find { case (_, definedSet) =>
            definedSet > 0 && (definedSet ^ 3) > 0
        }.map { case(i, definedSet) =>
            val isDefined = (definedSet & (1 << 1)) > 0
            Literal(i, isDefined)
        }
    }

    @tailrec private def unitPropagation(cnf: Seq[Clause], assignment: Seq[Literal]): (Seq[Clause], Seq[Literal]) = {
        val maybeUnit = findUnit(cnf)
        maybeUnit match {
            case None => (cnf, assignment)
            case Some(unit) => {
                val newCnf = propagate(cnf, unit)
                val newAssignment = assignment :+ unit
                unitPropagation(newCnf, newAssignment)
            }
        }
    }

    @tailrec private def pureLiteralElimination(cnf: Seq[Clause], assignment: Seq[Literal]): (Seq[Clause], Seq[Literal]) = {
        val maybePure = findPure(cnf)
        maybePure match {
            case None => (cnf, assignment)
            case Some(pure) => {
                val newCnf = propagate(cnf, pure)
                val newAssignment = assignment :+ pure
                pureLiteralElimination(newCnf, newAssignment)
            }
        }
    }

    // this is likely the place to try out new heuristics
    // for now, select the literal with the highest occurence in the cnf
    def chooseLiteral(cnf: Seq[Clause]): Int = {
        val flattened = cnf.flatMap(clause => clause.literals)

        val numToCountMap = flattened.foldLeft[Map[Int, Int]](Map()) { (map, literal) =>
            val Literal(i, _) = literal
            val count = map.getOrElse(i, 0)
            map + ((i, count + 1))
        }

        val (maxI, _) = numToCountMap.max
        maxI
    }

    def solveRecurse(cnf: Seq[Clause], assignment: Seq[Literal]): Option[Seq[Literal]] = {
        // unit propagation
        val (unitCnf, unitAssignment) = unitPropagation(cnf, assignment)

        // pure literal elimination
        val (pureCnf, pureAssignment) = pureLiteralElimination(unitCnf, unitAssignment)

        if (pureCnf.length == 0) {
            Some(pureAssignment)
        } else if (pureCnf.find(clause => clause.literals.isEmpty).isDefined) {
            None
        } else {
            // backtracking
            val maxI = chooseLiteral(pureCnf)
            val maybeInclude = solveRecurse(pureCnf :+ Clause(Seq(Literal(maxI, true))), pureAssignment)
            maybeInclude match {
                case None => solveRecurse(pureCnf :+ Clause(Seq(Literal(maxI, false))), pureAssignment)
                case _ => maybeInclude
            }
        }
    }

    def solve(cnf: Seq[Clause]): Option[Seq[Literal]] = solveRecurse(cnf, Seq())
}
