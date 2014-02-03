package joos.parsetree

abstract class ParseTreeNode {
  def symbol: String
}

case class TreeNode(val symbol: String, val children: Vector[ParseTreeNode]) extends ParseTreeNode

case class LeafNode(val symbol: String) extends ParseTreeNode