package joos

import java.io.FileInputStream
import org.scalatest.{Matchers, FlatSpec}

class LrOneReaderSpec extends FlatSpec with Matchers {
  final val sampleFileName = "/sample.lr1"

  final val parseTable = LrOneReader(new FileInputStream(getClass.getResource(sampleFileName).getPath))

  behavior of "Parse Table Reader"
  it should "recognize the number of tokens" in {
    parseTable.numTerminals shouldEqual 6
  }

  it should "recognize the number of non tokens" in {
    parseTable.numNonTerminals shouldEqual 3
  }

  it should "recognize the start symbol" in {
    parseTable.startSymbol shouldEqual "S"
  }

  it should "recognize the number of production rules" in {
    parseTable.numProductionRules shouldEqual 5
  }

  it should "recognize the number of states" in {
    parseTable.numStates shouldEqual 11
  }

  it should "recognize the number of parse actions" in {
    parseTable.numActions shouldEqual 28
  }
}