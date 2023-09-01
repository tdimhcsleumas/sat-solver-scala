package tdimhcsleumas.sat.solver.lib.algorithms

import scala.annotation.tailrec
import scala.math
import org.log4s._

class CDCLAlg extends AlgTrait {
    private[this] val logger = getLogger

    override def solve(cnf: Seq[Seq[Int]]): Option[Seq[Int]] = ???
}