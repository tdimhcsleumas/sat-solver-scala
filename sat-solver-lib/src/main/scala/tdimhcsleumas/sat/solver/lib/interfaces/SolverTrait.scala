package tdimhcsleumas.sat.solver.lib.interfaces

trait SolverTrait {
    def solve(cnf: Seq[Seq[Int]]): Option[Seq[Int]]
}
