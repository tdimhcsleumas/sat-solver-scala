package tdimhcsleumas.sat.solver.lib.domain

sealed trait Var
case class GenericVar[A](i: A) extends Var
