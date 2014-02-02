package joos

import org.scalatest.{FunSpec, Matchers}
import joos.parsetree.{ParseTree, ParseTreeNode, TerminalNode, NonTerminalNode}
import java.util

class ParseTreeSpec extends FunSpec with Matchers {
  describe("A parse tree") {
    it("should be able to represent simple production rules") {
      val id = TerminalNode("ID", "foobar")
      val producedSymbols = new util.ArrayList[ParseTreeNode]()
      producedSymbols.add(id)
      val term = NonTerminalNode("TERM", producedSymbols)
      val parseTree = ParseTree(term)

      parseTree.getRoot should be (term)
      parseTree.getRoot.asInstanceOf[NonTerminalNode].getProducedSymboles should contain (id)
    }
  }

}
