package tdimhcsleumas.sat.solver.cli

import tdimhcsleumas.sat.solver.lib.domain._

object ConjParser {
    def fromLines(lines: Iterator[String])  = {
        val (nums, clauses) = lines.foldLeft((Set[GenericVar[Int]](), Seq[Clause[GenericVar[Int]]]())){ (state, line) => 
            val (varSet, clauses) = state
            if (line == "" || line(0) == 'c' || line(0) == 'p') state
            else {
                val (nextSet, clause) = line.split(" ")
                    .filter(_.length > 0)
                    .map(_.toInt)
                    .filter(_ != 0)
                    .foldLeft((varSet, Seq[(GenericVar[Int], Asg)]())) { (prev, next) =>
                        val (prevSet, prevSeq) = prev
                        val nextSeq = prevSeq :+ ((GenericVar(next.abs.toInt), if (next >= 0) True else False))
                        (prevSet + GenericVar(next.abs.toInt), nextSeq)
                    }
                (nextSet, clauses :+ Clause(clause))
            } 
        }
        (nums, Conj(clauses))
    }
}