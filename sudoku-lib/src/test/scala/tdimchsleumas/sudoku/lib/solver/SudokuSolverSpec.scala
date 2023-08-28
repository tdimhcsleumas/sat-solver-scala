package tdimhcsleumas.sudoku.lib.solver

import tdimhcsleumas.sudoku.lib.domain._
import org.scalatest._
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Span, Minutes}
import funspec._

@Ignore
class SudokuSolverSpec extends AnyFunSpec with TimeLimitedTests {

    val timeLimit = Span(5, Minutes)

    it("solves a sudoku problem") {
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
            Seq(Some(9), Some(5), Some(7), Some(6), Some(1), Some(3), Some(2), Some(8), Some(4)),
            Seq(Some(4), Some(8), Some(3), Some(2), Some(5), Some(7), Some(1), Some(9), Some(6)),
            Seq(Some(6), Some(1), Some(2), Some(8), Some(4), Some(9), Some(5), Some(3), Some(7)),
            Seq(Some(1), Some(7), Some(8), Some(3), Some(6), Some(4), Some(9), Some(5), Some(2)),
            Seq(Some(5), Some(2), Some(4), Some(9), Some(7), Some(1), Some(3), Some(6), Some(8)),
            Seq(Some(3), Some(6), Some(9), Some(5), Some(2), Some(8), Some(7), Some(4), Some(1)),
            Seq(Some(8), Some(4), Some(5), Some(7), Some(9), Some(2), Some(6), Some(1), Some(3)),
            Seq(Some(2), Some(9), Some(1), Some(4), Some(3), Some(6), Some(8), Some(7), Some(5)),
            Seq(Some(7), Some(3), Some(6), Some(1), Some(8), Some(5), Some(4), Some(2), Some(9))
        )

        val solver = new SudokuSolver

        val maybeSolution = solver.solve(SudokuProblem.fromMat(problem))

        assert(maybeSolution.isDefined)

        val solution = maybeSolution.get.toMat
        
        assert(solution === expectedSolution)
    }
}