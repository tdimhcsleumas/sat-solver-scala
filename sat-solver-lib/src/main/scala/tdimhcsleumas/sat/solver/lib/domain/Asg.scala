package tdimhcsleumas.sat.solver.lib.domain

sealed trait Asg
case object True extends Asg
case object False extends Asg
