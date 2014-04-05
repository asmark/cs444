package joos.ast.expressions

import joos.ast.{ArrayAccessExpressionLike, AstConstructionException}
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}

case class ArrayAccessExpression(reference: Expression, index: Expression)
    extends Expression with ArrayAccessExpressionLike {
  override def toString = s"${reference}[${index}]"

  override def isArrayAccess = true
}

object ArrayAccessExpression {
  def apply(ptn: ParseTreeNode): ArrayAccessExpression = {
    ptn match {
      case TreeNode(ProductionRule("ArrayAccess", Seq(_, "[", "Expression", "]")), _, children) =>
        ArrayAccessExpression(Expression(children(0)), Expression(children(2)))
      case _ => throw new AstConstructionException("No valid production rule to make ArrayAccessExpression")
    }
  }
}
