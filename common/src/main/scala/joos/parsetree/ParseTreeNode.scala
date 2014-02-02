package joos.parsetree

import java.util

abstract class ParseTreeNode() {
  val itemType: String

  def getItemType = itemType
}

case class NonTerminalNode(itemType: String,
                           val producedSymbols: util.ArrayList[ParseTreeNode]) extends ParseTreeNode {
  def getProducedSymboles = producedSymbols
}

case class TerminalNode(itemType: String,
                        val value:Any) extends ParseTreeNode {
  def getValue = value
}