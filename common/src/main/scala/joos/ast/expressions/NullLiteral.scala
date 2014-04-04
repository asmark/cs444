package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.ast.types.PrimitiveType._
import joos.syntax.parsetree.{LeafNode, ParseTreeNode}
import joos.syntax.tokens.TokenKind
import joos.ast.types.NullType

class NullLiteral extends LiteralExpression {
  expressionType = NullType

  override def toString = expressionType.standardName
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
