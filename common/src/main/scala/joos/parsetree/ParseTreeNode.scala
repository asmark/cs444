package joos.parsetree

abstract class ParseTreeNode {
  def symbol: String
}

case class TreeNode(val symbol: String, val children: IndexedSeq[ParseTreeNode]) extends ParseTreeNode

case class LeafNode(val symbol: String) extends ParseTreeNode