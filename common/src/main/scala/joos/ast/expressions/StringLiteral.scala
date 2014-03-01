package joos.ast.expressions

import joos.ast.exceptions.AstConstructionException
import joos.parsetree.{LeafNode, ParseTreeNode}
import joos.tokens.{TokenKind, TerminalToken}

case class StringLiteral(token: TerminalToken) extends LiteralExpression

object StringLiteral {
  def apply(ptn: ParseTreeNode): StringLiteral = {
    ptn match {
      case LeafNode(token) if token.kind == TokenKind.StringLiteral =>
        StringLiteral(token)
      case _ => throw new AstConstructionException("No valid production rule to make StringLiteral")
    }
  }
}

