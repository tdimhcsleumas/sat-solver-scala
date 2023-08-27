package tdimhcsleumas.sat.solver.lib.domain

trait Var
case class GenericVar[A](i: A) extends Var
