package tdimhcsleumas.sudoku.lib.services

import tdimhcsleumas.sudoku.lib.domain._
import tdimhcsleumas.sat.solver.lib.domain._

class CnfCreatorService {
    private val problemSize = 9
    private val rootProblemSize = 3
    private val numbers = 1 to problemSize

    type SudokuVar = GenericVar[(Int, (Int, Int))]
    def SudokuVar(a: Int, point: (Int, Int)) = GenericVar(a, point)


    private def genCnf(points: Seq[(Int, Int)]): Seq[Clause] = {
        val permissiveClauses = points.map { point =>
            Clause(numbers.map(number => (SudokuVar(number, point), True)))
        }

        val pointRestrictiveClauses = points.flatMap { point =>
            numbers.zipWithIndex.flatMap { case (numI, i) =>
                numbers.zipWithIndex.flatMap { case (numJ, j) =>
                    if (i >= j) Seq()
                    else Seq(Clause(Seq(
                        (SudokuVar(numI, point), False),
                        (SudokuVar(numJ, point), False)
                    )))
                }
            }
        }

        val numberRestrictiveClauses = numbers.flatMap { number =>
            points.zipWithIndex.flatMap { case (pointI, i) =>
                points.zipWithIndex.flatMap { case (pointJ, j) =>
                    if (i >= j) Seq()
                    else Seq(Clause(Seq(
                        (SudokuVar(number, pointI), False),
                        (SudokuVar(number, pointJ), False)
                    )))
                }
            } 
        }
        
        permissiveClauses ++ pointRestrictiveClauses ++ numberRestrictiveClauses
    }

    private def genSquareGroup(startPoint: (Int, Int)): Seq[Clause] = {
        val (startRow, startCol) = startPoint

        val points = (startRow to startRow + rootProblemSize).flatMap { row =>
            (startCol to startCol + rootProblemSize).map { col =>
                (row, col)
            }
        }

        genCnf(points)
    }

    private def genRowGroup(startPoint: (Int, Int)): Seq[Clause] = {
        val (startRow, startCol) = startPoint

        val points = (startCol to problemSize).map { col =>
            (startRow, col)
        }

        genCnf(points)
    }

    private def genColGroup(startPoint: (Int, Int)): Seq[Clause] = {
        val (startRow, startCol) = startPoint

        val points = (startRow to problemSize).map { row =>
            (row, startCol)
        }

        genCnf(points)
    }

    def createCnf(problem: SudokuProblem): (Seq[Var], Conj) = {
        val variables = (1 to problemSize).flatMap { row =>
            (1 to problemSize).flatMap { col =>
                numbers.map(GenericVar(_, (row, col)))
            }
        }

        val squareGroups = (1 to problemSize by rootProblemSize).flatMap { row =>
            (1 to problemSize by rootProblemSize).flatMap { col =>
                genSquareGroup((row, col))
            }
        }

        val rowGroups = (1 to problemSize).flatMap { row =>
            genRowGroup((row, 0))
        }

        val colGroups = (1 to problemSize).flatMap { col =>
            genColGroup((0, col))
        }

        val cnf = Conj(squareGroups ++ rowGroups ++ colGroups)

        (variables, cnf)
    }
}
