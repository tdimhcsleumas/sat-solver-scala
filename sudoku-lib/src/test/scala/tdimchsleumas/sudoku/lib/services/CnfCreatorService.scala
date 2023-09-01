package tdimhcsleumas.sudoku.lib.services

import tdimhcsleumas.sudoku.lib.domain._
import tdimhcsleumas.sudoku.lib.solver._
import tdimhcsleumas.sat.solver.lib.domain._
import tdimhcsleumas.sat.solver.lib.solver._
import org.scalatest._
import funspec._

class CnfCreatorSpec extends AnyFunSpec {
    it("returns a cnf for a 4-doku problem") {
        val creatorService = new CnfCreatorService(4)

        val problem = SudokuProblem(Seq(
            Seq(Some(1), None, None, None),
            Seq(None, None, None, Some(2)),
            Seq(Some(3), None, None, None),
            Seq(None, None, Some(4), None),
        ))

        val cnf = creatorService.createCnf(problem)

        assert(cnf.clauses.length === 628)
    }

    it("returns a cnf for a sudoku problem") {
        val creatorService = new CnfCreatorService(4)

        val problem = SudokuProblem(Seq(
            Seq(None, None, None, None, None, None, Some(2), None, None),
            Seq(None, Some(8), None, None, None, Some(7), None, Some(9), None),
            Seq(Some(6), None, Some(2), None, None, None, Some(5), None, None),
            Seq(None, Some(7), None, None, Some(6), None, None, None, None),
            Seq(None, None, None, Some(9), None, Some(1), None, None, None),
            Seq(None, None, None, None, Some(2), None, None, Some(4), None),
            Seq(None, None, Some(5), None, None, None, Some(6), None, Some(3)),
            Seq(None, Some(9), None, Some(4), None, None, None, Some(7), None),
            Seq(None, None, Some(6), None, None, None, None, None, None)
        ))

        val cnf = creatorService.createCnf(problem)

        assert(cnf.clauses.length === 17759)
    }
}