package joos.parsetree

import joos.tokens.{TerminalToken, Token}
import joos.language.ProductionRule

abstract class ParseTreeNode {
  def token: Token
  def children: IndexedSeq[ParseTreeNode]
}

case class TreeNode(val productionRule: ProductionRule, token: Token, val children: IndexedSeq[ParseTreeNode]) extends ParseTreeNode

case class LeafNode(val token: TerminalToken) extends ParseTreeNode {
  override def children = IndexedSeq.empty
}