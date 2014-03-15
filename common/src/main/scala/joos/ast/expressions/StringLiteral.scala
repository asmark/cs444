package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.ast.types.StringType
import joos.syntax.parsetree.{LeafNode, ParseTreeNode}
import joos.syntax.tokens.{TokenKind, TerminalToken}

case class StringLiteral(token: TerminalToken) extends LiteralExpression {
  override def toString = s""""${token.lexeme}""""

  declarationType = StringType

}

object StringLiteral {
  def apply(ptn: ParseTreeNode): StringLiteral = {
    ptn match {
      case LeafNode(token) if token.kind == TokenKind.StringLiteral =>
        StringLiteral(token)
      case _ => throw new AstConstructionException("No valid production rule to make StringLiteral")
    }
  }
}

