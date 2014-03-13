package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.syntax.parsetree.{LeafNode, ParseTreeNode}
import joos.syntax.tokens.{TokenKind, TerminalToken}
import joos.ast.types.PrimitiveType

case class CharacterLiteral(token: TerminalToken) extends LiteralExpression {
  override def declarationType = PrimitiveType.CharacterType
}

object CharacterLiteral {
  def apply(ptn: ParseTreeNode): CharacterLiteral = {
    ptn match {
      case LeafNode(token) if token.kind == TokenKind.CharacterLiteral =>
        CharacterLiteral(token)
      case _ => throw new AstConstructionException("No valid production rule to make CharacterLiteral")
    }
  }
}