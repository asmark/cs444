package joos.weeder

import joos.parser.ParseMetaData
import joos.parsetree.{LeafNode, TreeNode, ParseTreeNode}
import scala.collection.mutable

case class CastExpressionWeeder() extends Weeder {
  override def check(ptn: ParseTreeNode, md: ParseMetaData): Unit = {
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
            case TreeNode(_,symbol, children) => {
              children.foreach(child => {
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
