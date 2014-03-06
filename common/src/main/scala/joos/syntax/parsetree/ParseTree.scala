package joos.syntax.parsetree

import scala.collection.mutable

class ParseTree(val root: ParseTreeNode) {

  def levelOrder: Vector[Seq[String]] = {
    var tree = mutable.MutableList.empty[Seq[String]]
    var levelDerivs = mutable.MutableList.empty[String]
    var currentLevel = 0

    val queue: mutable.Queue[(ParseTreeNode, Int)] = mutable.Queue((root, currentLevel))
    while (!queue.isEmpty) {
      val (node, level) = queue.dequeue()

      if (currentLevel < level) {
        tree += levelDerivs.toSeq
        levelDerivs = mutable.MutableList.empty[String]
        currentLevel = level
      }
      levelDerivs += node.token.symbol

      node match {
        case TreeNode(_,symbol, children) => {
          children.foreach(
            child => {
              queue.enqueue((child, level + 1))
            }
          )
        }
        case LeafNode(symbol) =>
      }

    }
    tree += levelDerivs.toSeq
    tree.toVector
  }
}

object ParseTree {
  def apply(root: ParseTreeNode) = {
    new ParseTree(root)
  }
}