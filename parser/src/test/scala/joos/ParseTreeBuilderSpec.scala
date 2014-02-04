package joos

import java.io.FileInputStream
import joos.exceptions.JoosParseException
import org.scalatest.{Matchers, FlatSpec}

class ParseTreeBuilderSpec extends FlatSpec with Matchers {

  final val sampleFileName = "/sample.lr1"
  final val actionTable = LrOneReader(new FileInputStream(getClass.getResource(sampleFileName).getPath)).actionTable

  "A valid sequence of tokens" should "result in a left-deep parse tree" in {
    val parseTreeBuilder = ParseTreeBuilder(actionTable)

    val tree = parseTreeBuilder.build(Seq("(", "id", "-", "(", "id", "-", "id", ")", ")")).levelOrder

    tree.length shouldEqual 8
    tree(0) shouldEqual Seq("expr")
    tree(1) shouldEqual Seq("term")
    tree(2) shouldEqual Seq("(", "expr", ")")
    tree(3) shouldEqual Seq("expr", "-", "term")
    tree(4) shouldEqual Seq("term", "(", "expr", ")")
    tree(5) shouldEqual Seq("id", "expr", "-", "term")
    tree(6) shouldEqual Seq("term", "id")
    tree(7) shouldEqual Seq("id")
  }

  "An invalid sequence of tokens" should "result in an exception" in {
    val parseTreeBuilder = ParseTreeBuilder(actionTable)

    intercept[JoosParseException] {
      parseTreeBuilder.build(Seq("(", "id", "-", "id", ")", ")"))
    }
  }
}
