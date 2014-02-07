package joos.weeder

import joos.parsetree.{LeafNode, TreeNode, ParseTreeNode}
import scala.collection.mutable
import joos.weeder.exceptions.WeederException

case class CastExpressionWeeder() extends Weeder {
  override def check(ptn: ParseTreeNode): Unit = {
    if (!ptn.token.symbol.equals(CastExpression))
      return

    var castTypeExpression: Option[ParseTreeNode] = None
    ptn.children.foreach(child => {
      if (child.token.symbol.equals(Expression)) {
        castTypeExpression = Some(child)
      }
    })

    castTypeExpression match {
      case Some(expr) =>
        val queue: mutable.Queue[ParseTreeNode] = mutable.Queue((expr))
        while (!queue.isEmpty) {
          val node = queue.dequeue()

          if (node.token.symbol.equals(Primary))
            throw new WeederException("Invalid CastExpression")

          node match {
            case TreeNode(symbol, children) => {
              children.foreach(child => {
                child.parent = node
                queue.enqueue(child)
              })
            }
            case LeafNode(symbol) =>
          }
        }
      case None => {}
    }

    return
  }
}
