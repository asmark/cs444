package joos.ast.expressions

import joos.ast.exceptions.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.ParseTreeNode
import joos.parsetree.TreeNode
import joos.tokens.TerminalToken

trait LiteralExpression extends Expression {
   def token: TerminalToken
 }

object LiteralExpression {
  def apply(ptn: ParseTreeNode): LiteralExpression = {
    ptn match {
      case TreeNode(ProductionRule("Literal", Seq("DecimalIntLiteral")), _, children) =>
        return IntegerLiteral(children(0))
      case TreeNode(ProductionRule("Literal", Seq("CharacterLiteral")), _, children) =>
        return CharacterLiteral(children(0))
      case TreeNode(ProductionRule("Literal", Seq("StringLiteral")), _, children) =>
        return StringLiteral(children(0))
      case TreeNode(ProductionRule("Literal", Seq("NullLiteral")), _, children) =>
        return NullLiteral(children(0))
      case TreeNode(ProductionRule("BooleanLiteral", _), _, children) =>
        return BooleanLiteral(children(0))
      case TreeNode(ProductionRule("Literal", Seq("BooleanLiteral")), _, children) =>
        return LiteralExpression(children(0))
      case _ => throw new AstConstructionException("No valid production rule to make LiteralExpression")
    }
  }
}
