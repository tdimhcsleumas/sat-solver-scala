package tdimhcsleumas.sat.solver.lib.domain

sealed trait Alg
case object DPLL extends Alg
case object CDCL extends Alg
case object Naive extends Alg
