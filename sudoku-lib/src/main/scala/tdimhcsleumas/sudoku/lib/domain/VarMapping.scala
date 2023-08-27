package tdimhcsleumas.sudoku.lib.domain

import tdimhcsleumas.sat.solver.lib.domain._

case class VarMapping(mapping: Map[Var, (Int, (Int, Int))])