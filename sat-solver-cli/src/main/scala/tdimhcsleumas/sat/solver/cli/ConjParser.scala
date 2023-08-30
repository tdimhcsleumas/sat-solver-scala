package tdimhcsleumas.sat.solver.cli

import tdimhcsleumas.sat.solver.lib.domain._

object ConjParser {
    def fromLines(lines: Iterator[String]): CNF[Int]  = {
        val cnfBuilder = CNF.builder[Int]()
        lines.foldLeft(cnfBuilder) { (builder, line) =>
            if (line == "" || line(0) == 'c' || line(0) == 'p') builder
            else {
                val literals = line.split(" ")
                    .filter(_.length > 0)
                    .map(_.toInt)
                    .filter(_ != 0)
                val clause = Clause(literals.map(num => Literal(num.abs, num > 0)).toList)
                builder.addClause(clause)
            }
        }.build()
    }
}