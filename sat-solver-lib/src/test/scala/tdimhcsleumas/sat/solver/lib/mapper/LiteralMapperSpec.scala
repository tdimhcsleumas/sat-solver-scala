package tdimhcsleumas.sat.solver.lib.mapper

import tdimhcsleumas.sat.solver.lib.domain._

import org.scalatest._
import funspec._

class LiteralMapperSpec extends AnyFunSpec {
    it("maps data correctly") {
        val firstVar = (1, (1, 1))
        val secondVar = (2, (1, 2))
        val thirdVar = (3, (1, 2))

        val cnf = CNF
            .builder()
            .addClause(Clause(Literal(firstVar, false), Literal(firstVar, true), Literal(secondVar, false)))
            .addClause(Clause(Literal(firstVar, true), Literal(thirdVar, false)))
            .addClause(Clause(Literal(thirdVar, true), Literal(secondVar, false)))
            .addClause(Clause(Literal(secondVar, true)))
            .build()

        val mapper = new LiteralMapper(cnf)

        assert(mapper.getInt(firstVar).get === 1)
        assert(mapper.getInt(secondVar).get === 2)
        assert(mapper.getInt(thirdVar).get === 3)

        assert(firstVar === mapper.getVariable(1).get)
        assert(secondVar === mapper.getVariable(2).get)
        assert(thirdVar === mapper.getVariable(3).get)
    }
}
