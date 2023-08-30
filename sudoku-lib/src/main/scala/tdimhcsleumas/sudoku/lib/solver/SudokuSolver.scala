package tdimhcsleumas.sudoku.lib.solver

import tdimhcsleumas.sudoku.lib.domain._
import tdimhcsleumas.sudoku.lib.services._
import tdimhcsleumas.sat.solver.lib.solver._
import tdimhcsleumas.sat.solver.lib.domain._

class SudokuSolver {
    def solve(problem: SudokuProblem): Option[SudokuSolution] = {
        val cnfCreatorService = new CnfCreatorService

        val solver = SatSolver.builder()
            .algorithm(DPLL)
            .build()

        val solverService = new SolverService(cnfCreatorService, solver)

        solverService.solve(problem)
    }
}