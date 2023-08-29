package tdimhcsleumas.sat.solver.lib.algorithms

trait AlgTrait {
    def solve(cnf: Seq[Seq[Int]]): Option[Seq[Int]]
}
