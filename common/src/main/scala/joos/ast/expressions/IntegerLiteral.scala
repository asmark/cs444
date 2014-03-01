package joos.ast.expressions

import joos.ast.exceptions.AstConstructionException
import joos.parsetree.{LeafNode, ParseTreeNode}
import joos.tokens.{TokenKind, TerminalToken}

case class IntegerLiteral(token: TerminalToken) extends LiteralExpression

object IntegerLiteral {
  def apply(ptn: ParseTreeNode): IntegerLiteral = {
    ptn match {
      case LeafNode(token) if token.kind == TokenKind.DecimalIntLiteral =>
        IntegerLiteral(token)
      case _ => throw new AstConstructionException("No valid production rule to make IntegerLiteral")
    }
  }
}
