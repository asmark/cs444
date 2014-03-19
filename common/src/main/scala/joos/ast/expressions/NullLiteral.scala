package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.ast.types.PrimitiveType._
import joos.syntax.parsetree.{LeafNode, ParseTreeNode}
import joos.syntax.tokens.TokenKind

class NullLiteral extends LiteralExpression {
  expressionType = NullType

  override def toString = NullType.name
}

object NullLiteral {
  private[this] final val instance = new NullLiteral

  def apply(ptn: ParseTreeNode): NullLiteral = {
    ptn match {
      case LeafNode(token) if token.kind == TokenKind.NullLiteral => instance
      case _ => throw new AstConstructionException("No valid production rule to make NullLiteral")
    }
  }
}
