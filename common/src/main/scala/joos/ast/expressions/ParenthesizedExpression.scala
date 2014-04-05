package joos.ast.expressions

import joos.ast.compositions.DeclarationLike
import joos.ast.{ArrayAccessExpressionLike, DeclarationReference, AstConstructionException}
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}

case class ParenthesizedExpression(expression: Expression)
    extends Expression
    with DeclarationReference[DeclarationLike]
    with ArrayAccessExpressionLike {
  override def toString = s"(${expression})"

  override lazy val isArrayAccess = {
    expression match {
      case arrayAccess: ArrayAccessExpressionLike => arrayAccess.isArrayAccess
      case _ => false
    }
  }
}

object ParenthesizedExpression {
  def apply(ptn: ParseTreeNode): ParenthesizedExpression = {
    ptn match {
      case TreeNode(ProductionRule("PrimaryNoNewArray", Seq("(", "Expression", ")")), _, children) =>
        ParenthesizedExpression(Expression(children(1)))
      case _ => throw new AstConstructionException("No valid production rule to create ParenthesizedExpression")
    }
  }
}
