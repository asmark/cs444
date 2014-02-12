package joos.parsetree

import joos.tokens.Token
import joos.language.ProductionRule

abstract class ParseTreeNode {
  def token: Token
  def children: IndexedSeq[ParseTreeNode]
  var parent: ParseTreeNode
}

case class TreeNode(val productionRule: ProductionRule, token: Token, val children: IndexedSeq[ParseTreeNode]) extends ParseTreeNode{
  override var parent: ParseTreeNode = null
}

case class LeafNode(val token: Token) extends ParseTreeNode {
  override def children = IndexedSeq.empty
  override var parent: ParseTreeNode = null
}