package tdimhcsleumas.sat.solver.lib.solver

import tdimhcsleumas.sat.solver.lib.algorithms._
import tdimhcsleumas.sat.solver.lib.domain._

class SatSolver(private val alg: AlgTrait) {
    def solve[A](cnf: CNF[A]): Option[Assignment[A]] = {
        val variables = cnf.clauses.flatMap(_.literals.map(_.variable))
            .toSet
            .toList

        val variableIntMapping = variables.zipWithIndex.foldLeft(Map[A, Int]()) { (map, pair) =>
            val (variable, index) = pair
            map + ((variable, index + 1))
        }

        // the first index shows up twice to account for off-by-one mapping
        val variableArray = variables.headOption match {
            case Some(head) => Vector[A](head) ++ variables.toVector
            case None => Vector[A]()
        }

        val intCnf = cnf.clauses.map { clause =>
            clause.literals.map { literal =>
                val Literal(variable, defined) = literal
                val int = variableIntMapping.getOrElse(variable, throw new Exception("could not find variable"))
                if (defined) {
                    int
                } else {
                    -1 * int
                }
            }
        }

        for {
            intSolution <- alg.solve(intCnf)
            unwrapped <- {
                val literals = intSolution.map { asgn => 
                    val variable = variableArray(asgn.abs)
                    Literal(variable, asgn > 0)
                }.toList

                Some(Assignment(literals))
            }
        } yield unwrapped
    }
}

object SatSolver {
    def builder(): SatSolverBuilder = new SatSolverBuilder(DPLL)
}

class SatSolverBuilder(private val alg: Alg) {

    def algorithm(newAlg: Alg): SatSolverBuilder = {
        new SatSolverBuilder(newAlg)
    }

    def build(): SatSolver = {
        val algImpl = AlgFactory.getAlg(alg)

        new SatSolver(algImpl)
    }
}