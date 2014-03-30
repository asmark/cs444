package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.ast.types.PrimitiveType._
import joos.core.Enumeration
import joos.syntax.parsetree.{LeafNode, ParseTreeNode}
import joos.syntax.tokens.TokenKind

class BooleanLiteral(val name: String) extends LiteralExpression with BooleanLiteral.Value {
  expressionType = BooleanType

  val value = if (this == BooleanLiteral.TrueLiteral) 1 else 0

}

object BooleanLiteral extends Enumeration {
  type T = BooleanLiteral

  final val TrueLiteral = this + new BooleanLiteral("true")
  final val FalseLiteral = this + new BooleanLiteral("false")

  def apply(ptn: ParseTreeNode): BooleanLiteral = {
    ptn match {
      case LeafNode(token) if token.kind == TokenKind.True => TrueLiteral
      case LeafNode(token) if token.kind == TokenKind.False => FalseLiteral
      case _ => throw new AstConstructionException("No valid production rule to make BooleanLiteral")
    }
  }
}
