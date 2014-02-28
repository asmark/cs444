package joos.ast.expressions

import joos.tokens.TerminalToken
import joos.parsetree.{LeafNode, TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException
import joos.semantic.{BlockEnvironment, TypeEnvironment, ModuleEnvironment}

case class PrefixExpression(operator: TerminalToken, operand: Expression) extends Expression

object PrefixExpression {
  def apply(ptn: ParseTreeNode)(
      implicit moduleEnvironment: ModuleEnvironment,
      typeEnvironment: TypeEnvironment,
      blockEnvironment: BlockEnvironment): PrefixExpression = {
    ptn match {
      case TreeNode(ProductionRule(_, Seq(_, "UnaryExpression")), _, Seq(LeafNode(operator), operand)) =>
        return PrefixExpression(operator, Expression(operand))
      case _ => throw new AstConstructionException("No valid production rule to create PrefixExpression")
    }
  }
}
