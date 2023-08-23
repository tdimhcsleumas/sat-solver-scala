package tdimhcsleumas.sat.solver.cli

import tdimhcsleumas.sat.solver.lib.domain._

object ConjParser {
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