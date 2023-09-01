package tdimhcsleumas.sudoku.lib.solver

import tdimhcsleumas.sudoku.lib.domain._
import org.scalatest._
import funspec._

class SudokuSolverSpec extends AnyFunSpec {

    // this takes like 30 mins to run
    ignore("solves a sudoku problem") {
        val problem = Seq(
            Seq(None, None, None, None, None, None, Some(2), None, None),
            Seq(None, Some(8), None, None, None, Some(7), None, Some(9), None),
            Seq(Some(6), None, Some(2), None, None, None, Some(5), None, None),
            Seq(None, Some(7), None, None, Some(6), None, None, None, None),
            Seq(None, None, None, Some(9), None, Some(1), None, None, None),
            Seq(None, None, None, None, Some(2), None, None, Some(4), None),
            Seq(None, None, Some(5), None, None, None, Some(6), None, Some(3)),
            Seq(None, Some(9), None, Some(4), None, None, None, Some(7), None),
            Seq(None, None, Some(6), None, None, None, None, None, None)
        )

        val expectedSolution = Seq(
            Seq(9, 5, 7, 6, 1, 3, 2, 8, 4),
            Seq(4, 8, 3, 2, 5, 7, 1, 9, 6),
            Seq(6, 1, 2, 8, 4, 9, 5, 3, 7),
            Seq(1, 7, 8, 3, 6, 4, 9, 5, 2),
            Seq(5, 2, 4, 9, 7, 1, 3, 6, 8),
            Seq(3, 6, 9, 5, 2, 8, 7, 4, 1),
            Seq(8, 4, 5, 7, 9, 2, 6, 1, 3),
            Seq(2, 9, 1, 4, 3, 6, 8, 7, 5),
            Seq(7, 3, 6, 1, 8, 5, 4, 2, 9),
        )

        val solver = new SudokuSolver(9)

        val maybeSolution = solver.solve(SudokuProblem.fromMat(problem))

        assert(maybeSolution.isDefined)

        val solution = maybeSolution.get.toMat
        
        assert(solution === expectedSolution)
    }

    it("solves a 4-doku problem") {
        val problem = Seq(
            Seq(Some(1), None, None, None),
            Seq(None, None, None, Some(2)),
            Seq(Some(3), None, None, None),
            Seq(None, None, Some(4), None),
        )

        val expectedSolution = Seq(
            Seq(1, 2, 3, 4),
            Seq(4, 3, 1, 2),
            Seq(3, 4, 2, 1),
            Seq(2, 1, 4, 3),
        )

        val solver = new SudokuSolver(4)

        val maybeSolution = solver.solve(SudokuProblem.fromMat(problem))

        assert(maybeSolution.isDefined)

        val solution = maybeSolution.get.toMat
        
        assert(solution === expectedSolution)
    }
}