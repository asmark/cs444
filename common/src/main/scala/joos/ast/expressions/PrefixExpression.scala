package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.{LeafNode, TreeNode, ParseTreeNode}
import joos.tokens.TerminalToken

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
