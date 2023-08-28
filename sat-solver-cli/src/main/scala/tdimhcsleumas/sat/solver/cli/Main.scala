package tdimhcsleumas.sat.solver.cli

import tdimhcsleumas.sat.solver.lib.solver.DPLLSolver

import scala.io.Source

object Main {
    def main(args: Array[String]): Unit = {
           args.lift(0)
            .fold(System.out.println("Usage: sat-solver <file> <algorithm>")) { filename =>
                val lines = Source.fromFile(filename).getLines()
                val conj = ConjParser.fromLines(lines)
                val solver = new DPLLSolver
                solver.solve(conj) match {
                    case Some(a) => println(s"Assignment: $a")
                    case None => println("Failed to find assignment!")
                }
            }
            
    }
}
