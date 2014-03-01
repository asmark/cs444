package joos.ast.expressions

import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException

case class FieldAccessExpression(expression: Expression, identifier: SimpleNameExpression) extends Expression

object FieldAccessExpression {
  def apply(ptn: ParseTreeNode): FieldAccessExpression = {
    ptn match {
      case TreeNode(ProductionRule("FieldAccess", Seq("Primary", ".", "Identifier")), _, children) =>
        FieldAccessExpression(Expression(children(0)), SimpleNameExpression(children(2)))
      case _ => throw new AstConstructionException("No valid production rule to create FieldAccessExpression")
    }
  }
}
