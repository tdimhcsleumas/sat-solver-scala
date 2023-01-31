import scala.io.Source

case class Var(i: Int)

sealed trait Asg
case object True extends Asg
case object False extends Asg

case class Solution(s: Map[Var, Asg])

case class Clause(s: Seq[(Var, Asg)])
case class Conj(c: Seq[Clause])

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

object ConjParser {
    def fromVec(data: Seq[Seq[Int]]): Conj = Conj(data
        .map (_.map { v =>
            (Var(v.abs.toInt), if (v >= 0) True else False)
        })
        .map(Clause(_)))

    def fromLines(lines: Iterator[String])  = {
        val (nums, clauses) = lines.foldLeft((Set[Var](), Seq[Clause]())){ (state, line) => 
            val (varSet, clauses) = state
            if (line == "" || line(0) == 'c' || line(0) == 'p') state
            else {
                val (nextSet, clause) = line.split(" ")
                    .filter(_.length > 0)
                    .map(_.toInt)
                    .filter(_ != 0)
                    .foldLeft((varSet, Seq[(Var, Asg)]())) { (prev, next) =>
                        val (prevSet, prevSeq) = prev
                        val nextSeq = prevSeq :+ ((Var(next.abs.toInt), if (next >= 0) True else False))
                        (prevSet + Var(next.abs.toInt), nextSeq)
                    }
                (nextSet, clauses :+ Clause(clause))
            } 
        }
        (nums, Conj(clauses))
    }
}

object Main {
    def test(data: (Set[Var], Conj)): Unit = Solver.solve(data._1, data._2)
        .fold(System.out.println("Failed to find assignment!")){a =>
            System.out.println(s"Assignment: $a")
        }

    def main(args: Array[String]): Unit = {
        args
            .lift(0)
            .fold(System.out.println("Usage: sat-solver <file>")) { filename =>
                val lines = Source.fromFile(filename).getLines()
                test(ConjParser.fromLines(lines))
            }
            
    }
}
