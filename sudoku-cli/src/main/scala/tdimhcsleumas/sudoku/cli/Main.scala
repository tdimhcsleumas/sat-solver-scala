package tdimhcsleumas.sudoku.cli

import com.monovore.decline._
import java.nio.file.{Path, Files}
import java.io.{BufferedWriter, OutputStreamWriter, FileWriter, BufferedReader, FileReader, InputStreamReader}
import java.lang.StringBuilder

import tdimhcsleumas.sudoku.lib.solver._
import tdimhcsleumas.sudoku.lib.domain._

import cats.implicits._
import cats.data.Validated

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

import scala.io.Source
import scala.io.StdIn
import org.log4s._

// sudoku-cli [ -f input.json ] [ -o output.json ]
// reads a file or stdin for a nested json array of integers
// either writes to stdout or file specified by the -o option

case class RunConfig(
    problem: List[List[Int]],
    outputFile: BufferedWriter
)

object Main extends CommandApp(
    name = "sudoku-lib",
    header = "Solve a json encoded sudoku problem. Numbers 1-9 are considered assigned and 0 is treated as unassigned.",
    main = {
        val logger = getLogger
        val fileOpt = Opts.option[String]("input", "Path to the input file.")
            .map(path => Source.fromFile(path).getLines())
            .withDefault(Iterator.continually(StdIn.readLine))
            .mapValidated { stream =>
                val lines = stream.takeWhile(_ != null).mkString("\n")
                decode[List[List[Int]]](lines) match {
                    case Left(e) => Validated.invalidNel(s"Failed to parse board: ${e.getMessage}")
                    case Right(s) => {
                        val allValid = s.foldLeft(true) { (cur, row) =>
                            cur && row.length == 9 && row.find(num => num < 0 || num > 9).isEmpty
                        }
                        if (s.length != 9 || !allValid) {
                            Validated.invalidNel(s"Failed to parse board: board must be 9 x 9 and must contain numbers between 0 and 9.")
                        } else {                        
                            Validated.valid(s)
                        }
                    }
                }
            }
            .mapValidated { board =>
                board.flatMap { row =>
                    row.filter(num => num < 0 || num > 9)
                } match {
                    case Nil => Validated.valid(board)
                    case _ => Validated.invalidNel("Board contains invalid number")
                }
            }

        val outputOpt = Opts.option[Path]("output", "Path to the input file.")
            .map(path => Files.newBufferedWriter(path))
            .withDefault(new BufferedWriter(new OutputStreamWriter(System.out)))


        (fileOpt, outputOpt).mapN(RunConfig.apply).map { config =>
            val RunConfig(rawProblem, sink) = config

            val problem = rawProblem.map { row =>
                row.map { num =>
                    if (num == 0) {
                        None
                    } else {
                        Some(num)
                    }
                }
            }

            val solver = SudokuSolver.builder.build()

            solver.solve(SudokuProblem.fromMat(problem)) match {
                case Some(solution) => sink.write(solution.toMat.asJson.noSpaces)
                case None => logger.warn("Could not determine solution.")
            }
        } 
    }
)
