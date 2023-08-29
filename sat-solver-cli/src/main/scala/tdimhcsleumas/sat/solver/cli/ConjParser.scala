package tdimhcsleumas.sat.solver.cli

object ConjParser {
    def fromLines(lines: Iterator[String]): Seq[Seq[Int]]  = {
        lines.flatMap { line =>
            if (line == "" || line(0) == 'c' || line(0) == 'p') List()
            else {
                val literals = line.split(" ")
                    .filter(_.length > 0)
                    .map(_.toInt)
                    .filter(_ != 0)
                List(literals.toSeq) 
            }
        }.toSeq
    }
}