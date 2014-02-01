package joos.parsetree

import scala.collection.mutable

abstract class ParseTreeNode() {
  val itemType: String

  def getItemType = itemType
}

case class NonTerminalNode(itemType: String,
                           val producedSymbols: mutable.LinkedHashMap[String, ParseTreeNode]) extends ParseTreeNode {
  def getProducedSymboles = producedSymbols
}
case class TerminalNode(itemType: String,
                        val value:Any) extends ParseTreeNode {
  def getValue = value
}