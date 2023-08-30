package tdimhcsleumas.sat.solver.lib.domain

case class CNF[A](clauses: List[Clause[A]])

object CNF {
    def builder[A](): CNFBuilder[A] = new CNFBuilder[A]()

    def apply[A](clauses: Clause[A]*): CNF[A] = new CNF(clauses.toList)
}

class CNFBuilder[A](private val clauses: List[Clause[A]] = List()) {
    def addClause(clause: Clause[A]): CNFBuilder[A] = new CNFBuilder(this.clauses :+ clause)

    def addClauses(_clauses: Seq[Clause[A]]): CNFBuilder[A] = new CNFBuilder(this.clauses ++ _clauses)

    def build(): CNF[A] = CNF(clauses)
}
