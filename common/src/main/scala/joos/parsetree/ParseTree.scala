package joos.parsetree

import scala.collection.mutable

class ParseTree(val root: ParseTreeNode) {

  def prettyFormat: String = {
    val buffer = new StringBuilder
    levelOrder.foreach(
      item =>
        buffer append (item.toString() + "\n")
    )

    buffer.toString
  }

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
      levelDerivs += node.token.symbol + "_" + currentLevel + "_" + (if (node.parent != null) node
        .parent
        .token
        .symbol
      else null)

      node match {
        case TreeNode(symbol, children) => {
          children.foreach(
            child => {
              child.parent = node
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