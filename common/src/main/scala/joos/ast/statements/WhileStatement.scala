package joos.ast

import joos.ast.expressions.Expression
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException

case class WhileStatement(cond: Expression, body: Statement) extends Statement

object WhileStatement {
  def apply(ptn: ParseTreeNode): WhileStatement = {
    ptn match {
      case TreeNode(ProductionRule(_, Seq("while", "(", "Expression", ")", _)), _,  children) =>
        WhileStatement(Expression(children(2)), Statement(children(4)))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create WhileStatement"
      )
    }
  }
}