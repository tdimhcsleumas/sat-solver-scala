package tdimhcsleumas.sat.solver.lib.domain

case class Solution[A <: Var](s: Map[A, Asg])
