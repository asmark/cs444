package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.syntax.parsetree.{LeafNode, ParseTreeNode}
import joos.syntax.tokens.{TokenKind, TerminalToken}
import joos.ast.types.{SimpleType, Type}
import joos.ast.compositions.LikeTyped

case class StringLiteral(token: TerminalToken) extends LiteralExpression with LikeTyped {
  override def declarationType: Type = SimpleType(NameExpression("java.lang.String"))
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

