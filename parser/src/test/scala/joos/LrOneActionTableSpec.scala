package joos

import java.io.FileInputStream
import joos.exceptions.{ShiftException, ReduceException}
import org.scalatest.{Matchers, FlatSpec}

class LrOneActionTableSpec extends FlatSpec with Matchers {

  final val sampleFileName = "/sample.lr1"

  final val actionTable = LrOneReader(new FileInputStream(getClass.getResource(sampleFileName).getPath)).actionTable

  behavior of "Action Table"
  it should "determine valid reduce states" in {
    actionTable.isReduce(8, "EOF") shouldEqual true
    actionTable.isReduce(8, "-") shouldEqual true
    actionTable.isReduce(8, ")") shouldEqual true
    actionTable.isReduce(0, "BOF") shouldEqual false
    actionTable.isReduce(8, "A") shouldEqual false
    actionTable.isReduce(13, "BOF") shouldEqual false
  }

  it should "determine valid shift states" in {
    actionTable.isShift(7, "-") shouldEqual true
    actionTable.isShift(7, ")") shouldEqual true
    actionTable.isShift(8, "EOF") shouldEqual false
    actionTable.isShift(8, "A") shouldEqual false
    actionTable.isShift(15, "BOF") shouldEqual false
  }

  behavior of "reduce"
  it should "determine the correct production rule on a valid reduce" in {
    val productionRule = actionTable.reduce(8, "EOF")
    productionRule.left shouldEqual ("expr")
    productionRule.right shouldEqual Seq("expr", "-", "term")
  }

  it should "throw an exception on an invalid reduce" in {
    intercept[ReduceException] {
      actionTable.reduce(13, "BOF")
    }
  }

  behavior of "shift"
  it should "determine the correct state on a valid shift" in {
    actionTable.shift(3, "id") shouldEqual 2
  }

  it should "throw an exception on an invalid shift" in {
    intercept[ShiftException] {
      actionTable.shift(13, "BOF")
    }
  }


}
