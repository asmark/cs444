package joos.ast.expressions

import joos.tokens.TerminalToken
import joos.parsetree.{LeafNode, TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException

case class PrefixExpression(operator: TerminalToken, operand: Expression) extends Expression

object PrefixExpression {
  def apply(ptn: ParseTreeNode): PrefixExpression = {
    ptn match {
      case TreeNode(ProductionRule(_, Seq(_, "UnaryExpression")), _, Seq(LeafNode(operator), operand)) =>
        PrefixExpression(operator, Expression(operand))
      case _ => throw new AstConstructionException("No valid production rule to create PrefixExpression")
    }
  }
}
