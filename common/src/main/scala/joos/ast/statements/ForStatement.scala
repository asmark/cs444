package joos.ast

import joos.ast.expressions.Expression
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException

case class ForStatement(
  forInit: Option[Expression],
  cond: Option[Expression],
  forUpdate: Option[Expression],
  body: Statement) extends Statement

object ForStatement {
  def apply(ptn: ParseTreeNode): ForStatement = {
    ptn match {
      case TreeNode(
          ProductionRule("ForStatement", production),
          _,
          children
        ) => {
        val init = production.indexOf("ForInit")
        val cond = production.indexOf("Expression")
        val update = production.indexOf("ForUpdate")
        var body = production.indexOf("Statement")
        if (body < 0)
          body = production.indexOf("StatementNoShortIf")

        return new ForStatement(
          if (init >= 0) Some(Expression(children(init))) else None,
          if (cond >= 0) Some(Expression(children(cond))) else None,
          if (update >= 0) Some(Expression(children(update))) else None,
          Statement(children(body))
        )
      }
      case _ => throw new AstConstructionException(
        "Invalid tree node to create ForStatement"
      )
    }
  }
}