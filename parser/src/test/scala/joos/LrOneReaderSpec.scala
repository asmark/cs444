package joos

import java.io.FileInputStream
import joos.parser.LrOneReader
import org.scalatest.{Matchers, FlatSpec}

class LrOneReaderSpec extends FlatSpec with Matchers {
  final val sampleFileName = "/sample.lr1"

  final val parseTable = LrOneReader(new FileInputStream(getClass.getResource(sampleFileName).getPath))

  behavior of "Parse Table Reader"
  it should "recognize the number of terminals" in {
    parseTable.numTerminals shouldEqual 6
  }

  it should "recognize the number of non terminals" in {
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

  behavior of "Joos Parse Table Reader"
  it should "parse the Joos LrOne file" ignore {
    lazy val lrOneParseTable = LrOneReader(new FileInputStream(getClass.getResource("/joos-1w-grammar.lr1").getPath))
    lrOneParseTable.numTerminals shouldEqual 104
    lrOneParseTable.numNonTerminals shouldEqual 137
    lrOneParseTable.startSymbol shouldEqual "Goal"
    lrOneParseTable.numProductionRules shouldEqual 356
    lrOneParseTable.numStates shouldEqual 628
    lrOneParseTable.numActions shouldEqual 14832
  }
}