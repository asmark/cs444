package joos.ast.expressions

import joos.ast.exceptions.AstConstructionException
import joos.parsetree.{LeafNode, ParseTreeNode}
import joos.tokens.{TokenKind, TerminalToken}

case class NullLiteral(token: TerminalToken) extends LiteralExpression

object NullLiteral {
  def apply(ptn: ParseTreeNode): NullLiteral = {
    ptn match {
      case LeafNode(token) if token.kind == TokenKind.NullLiteral =>
        NullLiteral(token)
      case _ => throw new AstConstructionException("No valid production rule to make NullLiteral")
    }
  }
}
