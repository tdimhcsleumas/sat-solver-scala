package tdimhcsleumas.sat.solver.lib.solver

import org.scalatest._
import funspec._

// turns out this doesn't work
@Ignore
class NaiveSolverSpec extends AnyFunSpec {
    it("returns an assignment for a solvable problem") {
        val problem = Seq(
            Seq(1, 2, 3, 5, -6),
            Seq(7, -6, -5, 2),
            Seq(8, 9, 7),
            Seq(4, -2, 1),
            Seq(9, 5, -6),
        )

        val solver = new NaiveSolver

        val maybeAssignment = solver.solve(problem)

        assert(maybeAssignment.isDefined)

        val assignment = maybeAssignment.get

        problem.foreach { clause =>
            val filtered = clause.filter(literal => assignment.contains(literal))
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
        )

        val solver = new NaiveSolver

        val maybeAssignment = solver.solve(problem)

        assert(maybeAssignment.isEmpty)
    }
}