package joos.ast.expressions

import joos.ast.{Operator, AstConstructionException}
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{LeafNode, TreeNode, ParseTreeNode}

case class InfixExpression(left: Expression, operator: Operator, right: Expression) extends Expression {
  override def toString = s"${left} ${operator} ${right}"
}

object InfixExpression {
  def apply(ptn: ParseTreeNode): InfixExpression = {
    ptn match {
      case TreeNode(
      ProductionRule(
      "ConditionalOrExpression"
      | "ConditionalAndExpression"
      | "InclusiveOrExpression"
      | "ExclusiveOrExpression"
      | "AndExpression"
      | "EqualityExpression"
      | "RelationalExpression"
      | "AdditiveExpression"
      | "MultiplicativeExpression", _), _, Seq(left, LeafNode(operator), right)) =>
        InfixExpression(Expression(left), Operator.fromName(operator.lexeme), Expression(right))
      case _ => throw new AstConstructionException("No valid production rule to create InfixExpression")
    }
  }
}
