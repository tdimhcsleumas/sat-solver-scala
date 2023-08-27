package tdimhcsleumas.sudoku.lib.domain

final case class SudokuSolution(mat: Seq[Seq[Int]]) {
    def toMat: Seq[Seq[Int]] = mat
}
