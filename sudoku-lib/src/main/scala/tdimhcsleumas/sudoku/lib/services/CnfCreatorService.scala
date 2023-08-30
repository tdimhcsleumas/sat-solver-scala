package tdimhcsleumas.sudoku.lib.services

import tdimhcsleumas.sudoku.lib.domain._
import tdimhcsleumas.sat.solver.lib.domain._
import org.log4s._

class CnfCreatorService {
    private[this] val logger = getLogger

    private val problemSize = 9
    private val rootProblemSize = 3
    private val numbers = 1 to problemSize


    private def genCnf(points: Seq[(Int, Int)]): Seq[Clause[(Int, (Int, Int))]] = {
        val permissiveClauses = points.map { point =>
            Clause(numbers.map(number => Literal((number, point), true)).toList)
        }

        val pointRestrictiveClauses = points.flatMap { point =>
            numbers.zipWithIndex.flatMap { case (numI, i) =>
                numbers.zipWithIndex.flatMap { case (numJ, j) =>
                    if (i >= j) Seq()
                    else Seq(Clause(
                        Literal((numI, point), false),
                        Literal((numJ, point), false)
                    ))
                }
            }
        }

        val numberRestrictiveClauses = numbers.flatMap { number =>
            points.zipWithIndex.flatMap { case (pointI, i) =>
                points.zipWithIndex.flatMap { case (pointJ, j) =>
                    if (i >= j) Seq()
                    else Seq(Clause(
                        Literal((number, pointI), false),
                        Literal((number, pointJ), false)
                    ))
                }
            } 
        }
        
        permissiveClauses ++ pointRestrictiveClauses ++ numberRestrictiveClauses
    }

    private def genSquareGroup(startPoint: (Int, Int)): Seq[Clause[(Int, (Int, Int))]] = {
        val (startRow, startCol) = startPoint

        val points = (startRow to startRow + rootProblemSize).flatMap { row =>
            (startCol to startCol + rootProblemSize).map { col =>
                (row, col)
            }
        }

        genCnf(points)
    }

    private def genRowGroup(startPoint: (Int, Int)): Seq[Clause[(Int, (Int, Int))]] = {
        val (startRow, startCol) = startPoint

        val points = (startCol to problemSize).map { col =>
            (startRow, col)
        }

        genCnf(points)
    }

    private def genColGroup(startPoint: (Int, Int)): Seq[Clause[(Int, (Int, Int))]] = {
        val (startRow, startCol) = startPoint

        val points = (startRow to problemSize).map { row =>
            (row, startCol)
        }

        genCnf(points)
    }

    def createCnf(problem: SudokuProblem): CNF[(Int, (Int, Int))] = {
        val variables = (1 to problemSize).flatMap { row =>
            (1 to problemSize).flatMap { col =>
                numbers.map((_, (row, col)))
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

        val unitClauses = problem.mat.zipWithIndex.flatMap { case (row, i) =>
            row.zipWithIndex.flatMap { case (maybeCol, j) =>
                maybeCol.map(col => Clause(Literal((col, (i, j)), true)))
            }
        }

        val cnf = CNF.builder()
            .addClauses(squareGroups)
            .addClauses(rowGroups)
            .addClauses(colGroups)
            .addClauses(unitClauses)
            .build()

        logger.info(s"Created cnf with ${variables.length} variables and ${cnf.clauses.length} clauses")
        cnf
    }
}
