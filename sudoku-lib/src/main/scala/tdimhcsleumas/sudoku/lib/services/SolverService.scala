package tdimhcsleumas.sudoku.lib.services

import tdimhcsleumas.sudoku.lib.domain._
import tdimhcsleumas.sat.solver.lib.domain._
import tdimhcsleumas.sudoku.lib.utils.SeqUtils._
import tdimhcsleumas.sat.solver.lib.solver.Solver

class SolverService(cnfCreatorService: CnfCreatorService) {
    def solve(problem: SudokuProblem): Option[SudokuSolution] = {
        val (vars, cnf) = cnfCreatorService.createCnf(problem)

        for {
            solution <- Solver.solve(vars, cnf)
            assignments <- Some(
                solution.s.foldLeft(Map[(Int, Int), Int]()) { (map, kv) =>
                    val (GenericVar((num, point)), asg) = kv
                    asg match {
                        case True => map + ((point, num))
                        case False => map
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