package tdimhcsleumas.sudoku.lib.utils

object SeqUtils {
    implicit class SeqHelper[A](seq: Seq[Option[A]]) {
        def all: Option[Seq[A]] = seq.foldLeft[Option[Seq[A]]](Some(Seq[A]())) { (maybeNewSeq, maybeElem) =>
            maybeNewSeq.flatMap { newSeq =>
                maybeElem.map(elem => newSeq :+ elem)
            }
        }
    }
}
