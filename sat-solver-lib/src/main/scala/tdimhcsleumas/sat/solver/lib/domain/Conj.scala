package tdimhcsleumas.sat.solver.lib.domain

case class Clause(s: Seq[(Var, Asg)])
case class Conj(c: Seq[Clause])
