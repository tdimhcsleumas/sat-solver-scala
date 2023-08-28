package tdimhcsleumas.sat.solver.cli

import tdimhcsleumas.sat.solver.lib.solver._
import scala.math

object ConjParser {
    def fromLines(lines: Iterator[String])  = {
        lines.flatMap { line =>
            if (line == "" || line(0) == 'c' || line(0) == 'p') List()
            else {
                val literals = line.split(" ")
                    .filter(_.length > 0)
                    .map(_.toInt)
                    .filter(_ != 0)
                    .map(num => Literal(math.abs(num), num > 0))
                List(Clause(literals))
            }
        }.toSeq
    }
}