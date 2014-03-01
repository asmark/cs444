package joos.ast.expressions

import joos.ast.exceptions.AstConstructionException
import joos.parsetree.{LeafNode, ParseTreeNode}
import joos.tokens.{TokenKind, TerminalToken}

case class CharacterLiteral(token: TerminalToken) extends LiteralExpression

object CharacterLiteral {
  def apply(ptn: ParseTreeNode): CharacterLiteral = {
    ptn match {
      case LeafNode(token) if token.kind == TokenKind.CharacterLiteral =>
        CharacterLiteral(token)
      case _ => throw new AstConstructionException("No valid production rule to make CharacterLiteral")
    }
  }
}