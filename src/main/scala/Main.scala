import scala.io.Source

case class Var(i: Int)

sealed trait Asg
case object True extends Asg
case object False extends Asg

case class Solution(s: Map[Var, Asg])

case class Clause(s: Seq[(Var, Asg)])
case class Conj(c: Seq[Clause])

object Solver {

    def tryAssign(conj: Conj, sol: Solution, testAsg: (Var, Asg)): Option[(Conj, Solution)] = {
        val (variable, asg) = testAsg

        if (sol.s.getOrElse(variable, asg) != asg) None
        else {
            val s = sol.s + testAsg
            val c = conj.c.map(_.s).flatMap { clause =>
                if (clause.find(p1 => s.find(p2 => p1 == p2).isDefined).isDefined) Seq()
                else Seq(clause.filterNot({ case(k, v) => s.getOrElse(k, v) != v }))
            }.map(Clause(_))
            Some((Conj(c), Solution(s)))
        }
    }

    def solve(conj: Conj, sol: Solution): Option[(Conj, Solution)] = {
        val sorted = conj.c.sortBy(_.s.length)

        if (sorted.isEmpty) Some((conj, sol))
        else for {
            clause <- sorted.headOption
            first <- clause.s.foldLeft[Option[(Conj, Solution)]](None) {(prev, atom) =>
                // do not attempt the next variable unless the previous attempt failed.
                prev.orElse(for {
                    (conj, sol) <- tryAssign(conj, sol, atom)
                    result      <- solve(conj, sol)
                } yield result)
            }
        } yield first
    }
}

object ConjParser {
    def fromVec(data: Seq[Seq[Int]]): Conj = Conj(data
        .map (_.map { v =>
            (Var(v.abs.toInt), if (v >= 0) True else False)
        })
        .map(Clause(_)))

    def fromLines(lines: Iterator[String])  = Conj(lines.flatMap { line => 
            if (line == "") Seq()
            else if (line(0) == 'c' || line(0) == 'p') Seq()
            else Seq(line.split(" ")
                .filter(_.length > 0)
                .map(_.toInt)
                .filter(_ != 0)
                .map { v =>
                    (Var(v.abs.toInt), if (v >= 0) True else False)
                })
                .map(Clause(_))
        }.toSeq)
}

object Main {
    def test(data: Conj): Unit = Solver.solve(data, Solution(Map()))
        .fold(System.out.println("Failed to find assignment!")){a =>
            System.out.println(s"Assignment: $a")
        }

    def main(args: Array[String]): Unit = {
        args
            .lift(0)
            .fold(System.out.println("Usage: sat-solver <file>")) { filename =>
                val lines = Source.fromFile(filename).getLines()
                val conj = ConjParser.fromLines(lines) 
                test(conj)
            }
            
    }
}
