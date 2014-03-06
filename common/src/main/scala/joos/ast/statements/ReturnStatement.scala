package joos.ast.statements

import joos.ast.AstConstructionException
import joos.ast.expressions.Expression
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}

case class ReturnStatement(expression: Option[Expression]) extends Statement

object ReturnStatement {
  def apply(ptn: ParseTreeNode): ReturnStatement = {
    ptn match {
      case TreeNode(ProductionRule("ReturnStatement", Seq("return", "Expression", ";")), _, children) =>
        ReturnStatement(Some(Expression(children(1))))
      case TreeNode(ProductionRule("ReturnStatement", Seq("return", ";")), _, children) =>
        ReturnStatement(None)
      case _ => throw new AstConstructionException(
        "Invalid tree node to create ReturnStatetment"
      )
    }
  }
}
