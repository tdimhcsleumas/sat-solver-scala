package tdimhcsleumas.sat.solver.lib.solver

import tdimhcsleumas.sat.solver.lib.interfaces.SolverTrait

/**
 * This was the first attempt. Observe its flaws.
 * It does not work.
 */
class NaiveSolver extends SolverTrait {
    case class Var(i: Int)

    sealed trait Asg
    case object True extends Asg
    case object False extends Asg

    case class Solution(s: Map[Var, Asg])

    case class Clause(s: Seq[(Var, Asg)])
    case class Conj(c: Seq[Clause])

    private def tryAssign(conj: Conj, sol: Solution): Option[(Conj, Solution)] = {
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

    override def solve(cnf: Seq[Seq[Int]]): Option[Seq[Int]] = {
        val nums = cnf.flatMap { literals =>
            literals.map(literal => Var(literal.abs))
        }.toSet

        println(nums)

        val conj = Conj(cnf.map { literals =>
            val asgs = literals.map { literal =>
                val variable = Var(literal.abs)
                val asg = if (literal.abs > 0) {
                    True
                } else {
                    False
                }
                (variable, asg)
            }
            Clause(asgs)
        })

        val maybeResult = tryAssign(conj, Solution(Map()))

        println(maybeResult)

        maybeResult.map { result =>
            val (_, sol) = result
            val fullyAssigned = nums.filterNot(num => sol.s.contains(num))
                .foldLeft(sol)((sol, num) => Solution(sol.s + ((num, True))))

            fullyAssigned.s.map { case (absLiteral, isAssigned) =>
                isAssigned match {
                    case True => absLiteral.i
                    case False => -1 * absLiteral.i
                }
            }.toSeq
        }

    }

}