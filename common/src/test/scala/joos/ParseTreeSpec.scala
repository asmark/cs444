package joos

import org.scalatest.{Matchers, FlatSpec}
import joos.parsetree.{ParseTree, ParseTreeNode, TerminalNode, NonTerminalNode}
import scala.collection.mutable

class ParseTreeSpec  extends FlatSpec with Matchers {
  behavior of "A simple tree to represent one rule"
  val id = TerminalNode("ID", "foobar")
  val producedSymbols = mutable.LinkedHashMap[String, ParseTreeNode](id.getItemType -> id)
  val term = NonTerminalNode("TERM", producedSymbols)
  val parseTree = ParseTree(term)
}
