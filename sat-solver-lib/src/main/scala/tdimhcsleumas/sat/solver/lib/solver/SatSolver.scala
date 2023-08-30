package tdimhcsleumas.sat.solver.lib.solver

import tdimhcsleumas.sat.solver.lib.algorithms._
import tdimhcsleumas.sat.solver.lib.domain._
import tdimhcsleumas.sat.solver.lib.mapper._
import org.log4s._

class SatSolver(private val alg: AlgTrait) {
    private[this] val logger = getLogger

    def solve[A](cnf: CNF[A]): Option[Assignment[A]] = {
        logger.info(f"Solving cnf with ${cnf.clauses.length} clauses with ${alg.getClass}")

        val mapper = new LiteralMapper(cnf)

        val intCnf = cnf.clauses.map { clause =>
            clause.literals.map { literal =>
                val Literal(variable, defined) = literal
                val int = mapper.getInt(variable).get
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
                    val variable = mapper.getVariable(asgn.abs).get
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