package tdimhcsleumas.sat.solver.cli

import tdimhcsleumas.sat.solver.lib.domain._
import tdimhcsleumas.sat.solver.lib.solver._

import scala.io.Source

object Main {
    def main(args: Array[String]): Unit = {
        val result = for {
            (file, algorithm) <- args.toList match {
                case arg1 :: arg2 :: _ => Right((arg1, arg2))
                case _ => Left("Usage: sat-solver-cli <file> <algorithm>")
            }

            algImpl <- algorithm match {
                case "DPLL" => Right(DPLL)
                case "Naive" => Right(Naive)
                case _ => Left("algorithm must be one of DPLL, Naive.")
            }

            lines <- try {
                Right(Source.fromFile(file).getLines)
            } catch {
                case e: Exception => Left(s"Failed to read $file. Message: ${e.getMessage()}")
            }
            
            solution <- {
                val cnf = ConjParser.fromLines(lines)
                val solver = SatSolver.builder()
                    .algorithm(algImpl)
                    .build()
                
                solver.solve(cnf) match {
                    case Some(s) => Right(s"Assignment: $s")
                    case None => Left("Failed to find assignment!")
                }
            }
        } yield solution
            
        result match {
            case Right(s) => println(s)
            case Left(s) => println(s)
        }
    }
}
