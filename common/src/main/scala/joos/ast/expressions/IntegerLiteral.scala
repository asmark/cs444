package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.syntax.parsetree.{LeafNode, ParseTreeNode}
import joos.syntax.tokens.{TokenKind, TerminalToken}
import joos.ast.compositions.LikeTyped

case class IntegerLiteral(token: TerminalToken) extends LiteralExpression with LikeTyped

object IntegerLiteral {
  def apply(ptn: ParseTreeNode): IntegerLiteral = {
    ptn match {
      case LeafNode(token) if token.kind == TokenKind.DecimalIntLiteral =>
        IntegerLiteral(token)
      case _ => throw new AstConstructionException("No valid production rule to make IntegerLiteral")
    }
  }
}
