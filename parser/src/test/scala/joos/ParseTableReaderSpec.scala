package joos

import org.scalatest.{Matchers, FlatSpec}
import scala.io.Source

class ParseTableReaderSpec extends FlatSpec with Matchers {
  final val sampleFileName = "/sample.lr1"

  "The reader" should "process the LR file properly" in {
    val reader = new ParseTableReader(Source.fromURL(getClass.getResource(sampleFileName)))
    val parseTable = reader.getParseTable
    parseTable.size should be (28)
    parseTable should contain (((8, "EOF"), (ParseAction.Reduce,2)))
    parseTable should contain (((6, "expr"), (ParseAction.Shift,10)))
  }
}