package tdimhcsleumas.sat.solver.lib.solver

import org.scalatest._
import funspec._
import scala.math

class DPLLSolverSpec extends AnyFunSpec {
    it("returns an assignment for a solvable problem") {
        val problem = Seq(
            Clause(Seq(Literal(1, true), Literal(2, true), Literal(3, true), Literal(5, true), Literal(6, false))),
            Clause(Seq(Literal(7, true), Literal(6, false), Literal(5, false), Literal(2, true))),
            Clause(Seq(Literal(8, true), Literal(9, false), Literal(7, true))),
            Clause(Seq(Literal(4, true), Literal(2, false), Literal(1, true))),
            Clause(Seq(Literal(9, true), Literal(5, true), Literal(6, false))),
        )

        val solver = new DPLLSolver

        val maybeAssignment = solver.solve(problem)

        assert(maybeAssignment.isDefined)

        val assignment = maybeAssignment.get

        problem.foreach { clause =>
            val filtered = clause.literals.filter(literal => assignment.contains(literal))
            assert(!filtered.isEmpty)
        }
    }

    it("retuns None when a problem is unsolvable") {
        val problem = Seq(
            Seq(1, 2, 3),
            Seq(1, 2, -3),
            Seq(1, -2, 3),
            Seq(-1, 2, 3),
            Seq(1, -2, -3),
            Seq(-1, -2, 3),
            Seq(-1, 2, -3),
            Seq(-1, -2, -3),
        ).map { row =>
            Clause(row.map(num => Literal(math.abs(num), num > 0)))
        }

        val solver = new DPLLSolver

        val maybeAssignment = solver.solve(problem)

        assert(maybeAssignment.isEmpty)
    }
}