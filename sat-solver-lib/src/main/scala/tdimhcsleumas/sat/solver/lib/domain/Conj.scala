package tdimhcsleumas.sat.solver.lib.domain

case class Clause[A <: Var](s: Seq[(A, Asg)])
case class Conj[A <: Var](c: Seq[Clause[A]])
