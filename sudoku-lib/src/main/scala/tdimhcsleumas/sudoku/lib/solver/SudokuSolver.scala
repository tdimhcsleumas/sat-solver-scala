package tdimhcsleumas.sudoku.lib.solver

import tdimhcsleumas.sudoku.lib.domain._
import tdimhcsleumas.sudoku.lib.services._

class SudokuSolver {
    def solve(problem: SudokuProblem): Option[SudokuSolution] = {
        val cnfCreatorService = new CnfCreatorService

        val solverService = new SolverService(cnfCreatorService)

        ???
    }
}