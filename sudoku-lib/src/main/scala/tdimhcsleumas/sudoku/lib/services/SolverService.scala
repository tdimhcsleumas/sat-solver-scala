package tdimhcsleumas.sudoku.lib.services

import tdimhcsleumas.sudoku.lib.domain._
import tdimhcsleumas.sat.solver.lib.solver.Solver

class SolverService(cnfCreatorService: CnfCreatorService) {
    def solve(problem: SudokuProblem): Option[SudokuSolution] = {
        val (vars, cnf) = cnfCreatorService.createCnf(problem)

        val maybeSolution = Solver.solve(vars, cnf)

        maybeSolution
        ???
    }
}