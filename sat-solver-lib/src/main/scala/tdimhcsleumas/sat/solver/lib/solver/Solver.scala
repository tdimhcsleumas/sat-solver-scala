package tdimhcsleumas.sat.solver.lib.solver

import tdimhcsleumas.sat.solver.lib.domain._

object Solver {

    def tryAssign(conj: Conj, sol: Solution): Option[(Conj, Solution)] = {
        val sorted = conj.c.sortBy(_.s.length)

        if (sorted.isEmpty) Some((conj, sol))
        else for {
            clause <- sorted.headOption
            first <- clause.s.foldLeft[Option[(Conj, Solution)]](None) {(prev, atom) =>
                val (variable, asg) = atom
                // do not attempt the next variable unless the previous attempt failed.
                prev.orElse(
                    if (sol.s.getOrElse(variable, asg) != asg) None
                    else {
                        val s = sol.s + atom
                        val c = conj.c.map(_.s).flatMap { clause =>
                            if (clause.find(p1 => s.find(p2 => p1 == p2).isDefined).isDefined) Seq()
                            else Seq(clause.filterNot({ case(k, v) => s.getOrElse(k, v) != v }))
                        }.map(Clause(_))
                        tryAssign(Conj(c), Solution(s))
                    }
                )
            }
        } yield first
    }

    def solve(nums: Set[Var], conj: Conj): Option[Solution] = tryAssign(conj, Solution(Map()))
        .map { result =>
            val (_, sol) = result
            nums
                .filterNot(num => sol.s.contains(num))
                .foldLeft(sol)((sol, num) => Solution(sol.s + ((num, True))))
        }
}