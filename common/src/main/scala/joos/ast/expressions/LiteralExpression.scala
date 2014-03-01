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
        IntegerLiteral(children(0))
      case TreeNode(ProductionRule("Literal", Seq("CharacterLiteral")), _, children) =>
        CharacterLiteral(children(0))
      case TreeNode(ProductionRule("Literal", Seq("StringLiteral")), _, children) =>
        StringLiteral(children(0))
      case TreeNode(ProductionRule("Literal", Seq("NullLiteral")), _, children) =>
        NullLiteral(children(0))
      case TreeNode(ProductionRule("BooleanLiteral", _), _, children) =>
        BooleanLiteral(children(0))
      case TreeNode(ProductionRule("Literal", Seq("BooleanLiteral")), _, children) =>
        LiteralExpression(children(0))
      case _ => throw new AstConstructionException("No valid production rule to make LiteralExpression")
    }
  }
}
