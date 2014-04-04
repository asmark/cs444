package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.syntax.parsetree.{LeafNode, ParseTreeNode}
import joos.syntax.tokens.{TokenKind, TerminalToken}
import joos.ast.types.PrimitiveType

case class IntegerLiteral(token: TerminalToken) extends LiteralExpression {
  expressionType = PrimitiveType.IntegerType

  override def toString = token.lexeme

  val value = if (token.lexeme.toLong equals 2147483648L) Integer.MIN_VALUE else token.lexeme.toInt
}

object IntegerLiteral {
  def apply(ptn: ParseTreeNode): IntegerLiteral = {
    ptn match {
      case LeafNode(token) if token.kind == TokenKind.DecimalIntLiteral =>
        IntegerLiteral(token)
      case _ => throw new AstConstructionException("No valid production rule to make IntegerLiteral")
    }
  }
}
