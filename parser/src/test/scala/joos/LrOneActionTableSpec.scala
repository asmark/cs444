package joos

import joos.exceptions.{ShiftException, ReduceException}
import org.scalatest.{Matchers, FlatSpec}

class LrOneActionTableSpec extends FlatSpec with Matchers {

  final val actionTable = LrOneReader(
    getClass.getResourceAsStream("/sample.lr1"),
    getClass.getResourceAsStream("/sample.cfg")
  )

  behavior of "Action Table"
  it should "determine valid reduce states" in {
    actionTable.isReduce(8, "eof") shouldEqual true
    actionTable.isReduce(8, "-") shouldEqual true
    actionTable.isReduce(8, ")") shouldEqual true
    actionTable.isReduce(0, "bof") shouldEqual false
    actionTable.isReduce(8, "A") shouldEqual false
    actionTable.isReduce(13, "bof") shouldEqual false
  }

  it should "determine valid shift states" in {
    actionTable.isShift(7, "-") shouldEqual true
    actionTable.isShift(7, ")") shouldEqual true
    actionTable.isShift(8, "eof") shouldEqual false
    actionTable.isShift(8, "A") shouldEqual false
    actionTable.isShift(15, "bof") shouldEqual false
  }

  it should "return the correct start symbol" in {
    actionTable.startSymbol shouldEqual "S"
  }

  behavior of "reduce"
  it should "determine the correct production rule on a valid reduce" in {
    val productionRule = actionTable.reduce(8, "eof")
    productionRule.base shouldEqual ("expr")
    productionRule.derivation shouldEqual Seq("expr", "-", "term")
  }

  it should "throw an exception on an invalid reduce" in {
    intercept[ReduceException] {
      actionTable.reduce(13, "bof")
    }
  }

  behavior of "shift"
  it should "determine the correct state on a valid shift" in {
    actionTable.shift(0, "bof") shouldEqual 6
  }

  it should "throw an exception on an invalid shift" in {
    intercept[ShiftException] {
      actionTable.shift(13, "bof")
    }
  }
}
