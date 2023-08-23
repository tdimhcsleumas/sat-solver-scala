package tdimhcsleumas.sat.solver.cli

import tdimhcsleumas.sat.solver.lib.solver.Solver

import scala.io.Source

object Main {
    def main(args: Array[String]): Unit = {
        args
            .lift(0)
            .fold(System.out.println("Usage: sat-solver <file>")) { filename =>
                val lines = Source.fromFile(filename).getLines()
                val (variables, conj) = ConjParser.fromLines(lines)
                Solver.solve(variables, conj) match {
                    case Some(a) => println(s"Assignment: $a")
                    case None => println("Failed to find assignment!")
                }
            }
            
    }
}
