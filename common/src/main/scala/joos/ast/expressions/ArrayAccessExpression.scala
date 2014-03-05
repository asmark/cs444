package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

case class ArrayAccessExpression(reference: Expression, index: Expression) extends Expression

object ArrayAccessExpression {
  def apply(ptn: ParseTreeNode): ArrayAccessExpression = {
    ptn match {
      case TreeNode(ProductionRule("ArrayAccess", Seq(_, "[", "Expression", "]")), _, children) =>
        return ArrayAccessExpression(Expression(children(0)), Expression(children(2)))
      case _ => throw new AstConstructionException("No valid production rule to make ArrayAccessExpression")
    }
  }
}
