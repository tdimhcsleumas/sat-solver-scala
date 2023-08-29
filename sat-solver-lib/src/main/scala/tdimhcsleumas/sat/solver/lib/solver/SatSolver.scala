package tdimhcsleumas.sat.solver.lib.solver

import tdimhcsleumas.sat.solver.lib.algorithms._
import tdimhcsleumas.sat.solver.lib.domain._

class SatSolver(private val alg: AlgTrait) {
    def solve[A](cnf: CNF[A]) = ???
}

object SatSolver {
    def builder(): SatSolverBuilder = new SatSolverBuilder(DPLL)
}

class SatSolverBuilder(private val alg: Alg) {

    class ConfigError(message: String) extends Exception(message) {}

    def algorithm(newAlg: Alg): SatSolverBuilder = {
        new SatSolverBuilder(newAlg)
    }

    def build(): SatSolver = {
        val algImpl = AlgFactory.getAlg(alg)

        new SatSolver(algImpl)
    }
}