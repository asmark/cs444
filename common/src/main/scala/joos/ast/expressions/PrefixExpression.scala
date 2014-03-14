package joos.ast.expressions

import joos.ast.{Operator, AstConstructionException}
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{LeafNode, TreeNode, ParseTreeNode}

case class PrefixExpression(operator: Operator, operand: Expression) extends Expression {
  override def toString = operator.name + operand.toString
}

object PrefixExpression {
  def apply(ptn: ParseTreeNode): PrefixExpression = {
    ptn match {
      case TreeNode(ProductionRule(_, Seq(_, "UnaryExpression")), _, Seq(LeafNode(operator), operand)) =>
        PrefixExpression(Operator.fromName(operator.lexeme), Expression(operand))
      case _ => throw new AstConstructionException("No valid production rule to create PrefixExpression")
    }
  }
}
