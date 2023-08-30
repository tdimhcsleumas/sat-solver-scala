package tdimhcsleumas.sudoku.lib.services

import tdimhcsleumas.sudoku.lib.domain._
import tdimhcsleumas.sat.solver.lib.domain._
import tdimhcsleumas.sudoku.lib.utils.SeqUtils._
import tdimhcsleumas.sat.solver.lib.solver.SatSolver

class SolverService(cnfCreatorService: CnfCreatorService, solver: SatSolver) {
    def solve(problem: SudokuProblem): Option[SudokuSolution] = {
        val cnf = cnfCreatorService.createCnf(problem)

        for {
            solution <- solver.solve(cnf)
            assignments <- Some(
                solution.literals.foldLeft(Map[(Int, Int), Int]()) { (map, kv) =>
                    val Literal((num, point), asg) = kv
                    if (asg) {
                        map + ((point, num))
                    } else {
                        map
                    }
                }
            )
            mat <- (1 to 9).map { row =>
                (1 to 9).map { col =>
                    assignments.get((row, col))
                }.all
            }.all
            sudokuSolution <- Some(SudokuSolution(mat))
        } yield sudokuSolution
    }
}