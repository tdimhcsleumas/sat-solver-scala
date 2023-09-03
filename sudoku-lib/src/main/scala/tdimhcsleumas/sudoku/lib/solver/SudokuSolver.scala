package tdimhcsleumas.sudoku.lib.solver

import tdimhcsleumas.sudoku.lib.domain._
import tdimhcsleumas.sudoku.lib.services._
import tdimhcsleumas.sat.solver.lib.solver._
import tdimhcsleumas.sat.solver.lib.domain._

class SudokuSolver(private val problemSize: Int = 9) {
    def solve(problem: SudokuProblem): Option[SudokuSolution] = {
        val cnfCreatorService = new CnfCreatorService(problemSize)

        val solver = SatSolver.builder()
            .algorithm(CDCL)
            .build()

        val solverService = new SolverService(problemSize, cnfCreatorService, solver)

        solverService.solve(problem)
    }
}

object SudokuSolver {
    def builder(): SudokuSolverBuilder = new SudokuSolverBuilder(9)
}

class SudokuSolverBuilder(problemSize: Int = 9) {
    def problemSize(_size: Int): SudokuSolverBuilder = new SudokuSolverBuilder(_size)

    def build(): SudokuSolver = {
        new SudokuSolver(problemSize)
    } 
}
