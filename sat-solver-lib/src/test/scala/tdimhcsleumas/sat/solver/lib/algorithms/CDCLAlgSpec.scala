package tdimhcsleumas.sat.solver.lib.algorithms

import org.scalatest._
import funspec._

class CDCLAlgSpec extends AnyFunSpec {
    it("returns an assignment for a solvable problem") {
        val problem = Seq(
          Seq(1, 2, 3, 5, -6),
          Seq(7, -6, -5, 2),
          Seq(8, 9, 7),
          Seq(4, -2, 1),
          Seq(9, 5, -6)
        )

        val solver = new CDCLAlg

        val maybeAssignment = solver.solve(problem)

        assert(maybeAssignment.isDefined)

        val assignment = maybeAssignment.get

        problem.foreach { clause =>
            val filtered = clause.filter(literal => assignment.contains(literal))
            assert(!filtered.isEmpty)
        }
    }

    it("analyzes conflict") {
        val cnf = Seq(
          Seq((1, true), (2, true), (3, true)),
          Seq((1, true), (2, true), (3, false)),
          Seq((2, false), (4, true)),
          Seq((1, true), (2, false), (4, false)),
          Seq((1, false), (5, true), (6, true)),
          Seq((1, false), (5, true), (6, false)),
          Seq((5, false), (6, false)),
          Seq((1, false), (5, false), (6, true))
        )

        val solver = new CDCLAlg

        val trail = Seq(
          solver.TrailEnt((1, false), 1, None),
          solver.TrailEnt((2, false), 2, None),
          solver.TrailEnt((3, true), 2, Some(Seq((1, true), (2, true), (3, true)))),
          solver.TrailEnt((0, true), 2, Some(Seq((1, true), (2, true), (3, false))))
        )

        val problem = solver.ProblemInstance(
          cnf = cnf,
          variables = Set(1, 2, 3, 4, 5, 6),
          assignment = Map(1 -> false, 2 -> false, 3 -> true),
          decisionLevel = 2,
          trail = trail
        )

        val analysis = solver.conflictAnalysis(problem, trail)

        assert(analysis.cnf.length === cnf.length + 1)
        assert(analysis.cnf.last === Seq((1, true), (2, true)))
        assert(analysis.decisionLevel === 1)
        assert(analysis.assignment === Map[Int, Boolean](1 -> false))
        assert(analysis.trail === Seq(solver.TrailEnt((1, false), 1, None)))
    }

    it("analyzes conflict single") {
        val cnf = Seq(
          Seq((1, true), (2, true), (3, true)),
          Seq((1, true), (2, true), (3, false)),
          Seq((2, false), (4, true)),
          Seq((1, true), (2, false), (4, false)),
          Seq((1, false), (5, true), (6, true)),
          Seq((1, false), (5, true), (6, false)),
          Seq((5, false), (6, false)),
          Seq((1, false), (5, false), (6, true)),
          Seq((1, true), (2, true))
        )

        val solver = new CDCLAlg

        val trail = Seq(
          solver.TrailEnt((1, false), 1, None),
          solver.TrailEnt(
            (2, true),
            2,
            Some(
              Seq((1, true), (2, true))
            )
          ),
          solver.TrailEnt((4, true), 2, Some(Seq((2, false), (4, true)))),
          solver.TrailEnt((0, true), 2, Some(Seq((1, true), (2, false), (4, false))))
        )

        val problem = solver.ProblemInstance(
          cnf = cnf,
          variables = Set(1, 2, 3, 4, 5, 6),
          assignment = Map(1 -> false, 2 -> true, 4 -> false),
          decisionLevel = 1,
          trail = trail
        )

        val analysis = solver.conflictAnalysis(problem, trail)

        assert(analysis.cnf.length === cnf.length + 1)
        assert(analysis.cnf.last === Seq((1, true)))
        assert(analysis.decisionLevel === 0)
        assert(analysis.assignment === Map[Int, Boolean]())
        assert(analysis.trail === Seq[solver.TrailEnt]())
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

        val solver = new CDCLAlg

        val maybeAssignment = solver.solve(problem)

        assert(maybeAssignment.isEmpty)
    }

    it("solves") {
        val problem = Seq(
          Seq(1, 4),
          Seq(1, -3, -8),
          Seq(1, 8, 12),
          Seq(2, 11),
          Seq(-7, -3, 9),
          Seq(-7, 8, -9),
          Seq(7, 8, -10),
          Seq(7, 10, -12)
        )

        val solver = new CDCLAlg

        val maybeAssignment = solver.solve(problem)

        assert(maybeAssignment.isDefined)

        val assignment = maybeAssignment.get

        problem.foreach { clause =>
            val filtered = clause.filter(literal => assignment.contains(literal))
            assert(!filtered.isEmpty)
        }
    }

    it("solves aim-50") {
        val problem = Seq(
          Seq(16, 17, 30),
          Seq(-17, 22, 30),
          Seq(-17, -22, 30),
          Seq(16, -30, 47),
          Seq(16, -30, -47),
          Seq(-16, -21, 31),
          Seq(-16, -21, -31),
          Seq(-16, 21, -28),
          Seq(-13, 21, 28),
          Seq(13, -16, 18),
          Seq(13, -18, -38),
          Seq(13, -18, -31),
          Seq(31, 38, 44),
          Seq(-8, 31, -44),
          Seq(8, -12, -44),
          Seq(8, 12, -27),
          Seq(12, 27, 40),
          Seq(-4, 27, -40),
          Seq(12, 23, -40),
          Seq(-3, 4, -23),
          Seq(3, -23, -49),
          Seq(3, -13, -49),
          Seq(-23, -26, 49),
          Seq(12, -34, 49),
          Seq(-12, 26, -34),
          Seq(19, 34, 36),
          Seq(-19, 26, 36),
          Seq(-30, 34, -36),
          Seq(24, 34, -36),
          Seq(-24, -36, 43),
          Seq(6, 42, -43),
          Seq(-24, 42, -43),
          Seq(-5, -24, -42),
          Seq(5, 20, -42),
          Seq(5, -7, -20),
          Seq(4, 7, 10),
          Seq(-4, 10, -20),
          Seq(7, -10, -41),
          Seq(-10, 41, 46),
          Seq(-33, 41, -46),
          Seq(33, -37, -46),
          Seq(32, 33, 37),
          Seq(6, -32, 37),
          Seq(-6, 25, -32),
          Seq(-6, -25, -48),
          Seq(-9, 28, 48),
          Seq(-9, -25, -28),
          Seq(19, -25, 48),
          Seq(2, 9, -19),
          Seq(-2, -19, 35),
          Seq(-2, 22, -35),
          Seq(-22, -35, 50),
          Seq(-17, -35, -50),
          Seq(-29, -35, -50),
          Seq(-1, 29, -50),
          Seq(1, 11, 29),
          Seq(-11, 17, -45),
          Seq(-11, 39, 45),
          Seq(-26, 39, 45),
          Seq(-3, -26, 45),
          Seq(-11, 15, -39),
          Seq(14, -15, -39),
          Seq(14, -15, -45),
          Seq(14, -15, -27),
          Seq(-14, -15, 47),
          Seq(17, 17, 40),
          Seq(1, -29, -31),
          Seq(-7, 32, 38),
          Seq(-14, -33, -47),
          Seq(-1, 2, -8),
          Seq(35, 43, 44),
          Seq(21, 21, 24),
          Seq(20, 29, -48),
          Seq(23, 35, -37),
          Seq(2, 18, -33),
          Seq(15, 25, -45),
          Seq(9, 14, -38),
          Seq(-5, 11, 50),
          Seq(-3, -13, 46),
          Seq(-13, -41, 43)
        )

        val solver = new CDCLAlg

        val maybeAssignment = solver.solve(problem)

        assert(maybeAssignment.isDefined)

        val assignment = maybeAssignment.get

        problem.foreach { clause =>
            val filtered = clause.filter(literal => assignment.contains(literal))
            assert(!filtered.isEmpty)
        }
    }
}
