package tdimhcsleumas.sudoku.lib.domain

final case class SudokuProblem(mat: Seq[Seq[Option[Int]]])

object SudokuProblem {
    def fromMat(mat: Seq[Seq[Option[Int]]]): SudokuProblem = {
        SudokuProblem(mat)
    }
}