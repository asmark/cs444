package joos.parsetree

import joos.language.ProductionRule
import joos.tokens.{TerminalToken, Token}

abstract class ParseTreeNode {
  def token: Token
  def children: IndexedSeq[ParseTreeNode]
}

case class TreeNode(productionRule: ProductionRule, token: Token, children: IndexedSeq[ParseTreeNode]) extends ParseTreeNode

case class LeafNode(token: TerminalToken) extends ParseTreeNode {
  override def children = IndexedSeq.empty
}