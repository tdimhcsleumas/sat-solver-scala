package tdimhcsleumas.sat.solver.lib.algorithms

import tdimhcsleumas.sat.solver.lib.domain._

object AlgFactory {
    def getAlg(alg: Alg): AlgTrait = alg match {
        case DPLL => new DPLLAlg
        case Naive => new NaiveAlg
        case CDCL => new CDCLAlg
    }
}
