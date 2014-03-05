package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}

case class ParenthesizedExpression(expression: Expression) extends Expression

object ParenthesizedExpression {
  def apply(ptn: ParseTreeNode): ParenthesizedExpression = {
    ptn match {
      case TreeNode(ProductionRule("PrimaryNoNewArray", Seq("(", "Expression", ")")), _, children) =>
        ParenthesizedExpression(Expression(children(1)))
      case _ => throw new AstConstructionException("No valid production rule to create ParenthesizedExpression")
    }
  }
}
