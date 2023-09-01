package tdimhcsleumas.sat.solver.lib.mapper

import tdimhcsleumas.sat.solver.lib.domain._

class LiteralMapper[A](cnf: CNF[A]) {
    private val variables =
        cnf.clauses.flatMap(_.literals.map(_.variable)).toSet.toList

    private val variableIntMapping =
        variables.zipWithIndex.foldLeft(Map[A, Int]()) { (map, pair) =>
            val (variable, index) = pair
            map + ((variable, index + 1))
        }

    // the first index shows up twice to account for off-by-one mapping
    private val variableArray = variables.headOption match {
        case Some(head) => Vector[A](head) ++ variables.toVector
        case None       => Vector[A]()
    }

    def getInt(literal: A): Option[Int] = {
        variableIntMapping.get(literal)
    }

    def getVariable(int: Int): Option[A] = {
        variableArray.lift(int)
    }
}
