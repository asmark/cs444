package joos.ast.expressions

import joos.tokens.TerminalToken
import joos.parsetree.{LeafNode, TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException

case class InfixExpression(left: Expression, operator: TerminalToken, right: Expression) extends Expression

object InfixExpression {
  def apply(ptn: ParseTreeNode): InfixExpression = {
    ptn match {
      case TreeNode(ProductionRule(
      "ConditionalOrExpression" | "ConditionalAndExpression" | "InclusiveOrExpression" |
      "ExclusiveOrExpression" | "AndExpression" | "EqualityExpression" | "RelationalExpression" |
      "AdditiveExpression" | "MultiplicativeExpression", _), _, Seq(left, LeafNode(operator), right)) =>
        InfixExpression(Expression(left), operator, Expression(right))
      case _ => throw new AstConstructionException("No valid production rule to create InfixExpression")
    }
  }
}
