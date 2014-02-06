package joos

import java.io.FileInputStream
import org.scalatest.{Matchers, FlatSpec}

class LrOneReaderSpec extends FlatSpec with Matchers {
  val parseTable = LrOneReader(
    getClass.getResourceAsStream("/sample.lr1"),
    getClass.getResourceAsStream("/sample.lr1"))

  it should "recognize the start symbol" in {
    parseTable.startSymbol shouldEqual "S"
  }

  it should "recognize the number of states" in {
    parseTable.statesCount shouldEqual 11
  }

  it should "recognize the number of parse actions" in {
    parseTable.parseActions.size shouldEqual 28
  }

  behavior of "Joos Parse Table Reader"
  it should "parse the Joos LrOne file" ignore {
    lazy val lrOneParseTable = LrOneReader(
      new FileInputStream(resources.lalr1Table),
      new FileInputStream(resources.serializedGrammar)
    )
    lrOneParseTable.startSymbol shouldEqual "Goal"
    lrOneParseTable.parseActions.size shouldEqual 7317
    lrOneParseTable.statesCount shouldEqual 419
  }
}
