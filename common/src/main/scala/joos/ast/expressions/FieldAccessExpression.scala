package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}

case class FieldAccessExpression(expression: Expression, identifier: SimpleNameExpression) extends Expression {
  override def toString = s"${expression}.${identifier}"
}

object FieldAccessExpression {
  def apply(ptn: ParseTreeNode): FieldAccessExpression = {
    ptn match {
      case TreeNode(ProductionRule("FieldAccess", Seq("Primary", ".", "Identifier")), _, children) =>
        FieldAccessExpression(Expression(children(0)), SimpleNameExpression(children(2)))
      case _ => throw new AstConstructionException("No valid production rule to create FieldAccessExpression")
    }
  }
}
