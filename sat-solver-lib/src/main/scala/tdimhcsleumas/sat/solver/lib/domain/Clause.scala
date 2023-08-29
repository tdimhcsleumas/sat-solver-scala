package tdimhcsleumas.sat.solver.lib.domain

case class Literal[A](variable: A, defined: Boolean)

case class Clause[A] private (literals: List[Literal[A]])

object Clause {
    def apply[A](literals: Literal[A]*): Clause[A] = new Clause[A](literals.toList)
}
