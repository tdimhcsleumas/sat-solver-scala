package tdimhcsleumas.sat.solver.lib.algorithms

import org.scalatest._
import funspec._

class DPLLAlgSpec extends AnyFunSpec {
    it("returns an assignment for a solvable problem") {
        val problem = Seq(
          Seq(1, 2, 3, 5, -6),
          Seq(7, -6, -5, 2),
          Seq(8, 9, 7),
          Seq(4, -2, 1),
          Seq(9, 5, -6)
        )

        val solver = new DPLLAlg

        val maybeAssignment = solver.solve(problem)

        assert(maybeAssignment.isDefined)

        val assignment = maybeAssignment.get

        problem.foreach { clause =>
            val filtered = clause.filter(literal => assignment.contains(literal))
            assert(!filtered.isEmpty)
        }
    }

    it("correctly finds unit") {
        val solver = new DPLLAlg

        val unassignedInstance = solver.ProblemInstance(
          cnf = Seq(
            Seq((1, true), (2, true), (3, true), (6, false)),
            Seq((5, true), (6, false)),
            Seq((9, true))
          ),
          variables = Set(1, 2, 3, 5, 6, 9),
          assignment = Map()
        )

        val maybeUnassigned = solver.findUnit(unassignedInstance)

        assert(maybeUnassigned.isDefined)

        assert(maybeUnassigned.get === (9, true))

        val assignedInstance = solver.ProblemInstance(
          cnf = Seq(
            Seq((1, true), (2, true), (3, true), (6, false)),
            Seq((5, true), (6, false)),
            Seq((4, true), (2, false), (1, true))
          ),
          variables = Set(1, 2, 3, 5, 6, 9),
          assignment = Map(4 -> false, 2 -> true)
        )

        val maybeAssigned = solver.findUnit(assignedInstance)

        assert(maybeAssigned.isDefined)

        assert(maybeAssigned.get === (1, true))

        val noneInstance = solver.ProblemInstance(
          cnf = Seq(
            Seq((1, true), (2, true), (3, true), (6, false)),
            Seq((5, true), (6, false)),
            Seq((4, true), (2, false), (1, true))
          ),
          variables = Set(1, 2, 3, 5, 6, 9),
          assignment = Map(4 -> true, 2 -> false)
        )

        val maybeNone = solver.findUnit(noneInstance)

        assert(maybeNone.isEmpty)

        val initialInstance = solver.ProblemInstance(
          cnf = Seq(
            Seq((1, true), (2, true), (3, true), (6, false)),
            Seq((5, true), (6, false)),
            Seq((4, true), (2, false), (1, true))
          ),
          variables = Set(1, 2, 3, 5, 6, 9),
          assignment = Map()
        )

        val maybeinitial = solver.findUnit(initialInstance)

        assert(maybeinitial.isEmpty)
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
          Seq(-1, -2, -3)
        )

        val solver = new DPLLAlg

        val maybeAssignment = solver.solve(problem)

        assert(maybeAssignment.isEmpty)
    }
}
