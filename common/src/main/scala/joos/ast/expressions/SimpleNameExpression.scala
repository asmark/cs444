package joos.ast.expressions

import joos.ast.exceptions.AstConstructionException
import joos.parsetree.{LeafNode, ParseTreeNode}
import joos.tokens.{TokenKind, TerminalToken}

case class SimpleNameExpression(identifier: TerminalToken) extends NameExpression

object SimpleNameExpression {
  def apply(ptn: ParseTreeNode): SimpleNameExpression = {
    ptn match {
      case LeafNode(token) if token.kind == TokenKind.Id =>
        return SimpleNameExpression(token)
      case _ => throw new AstConstructionException("No valid production rule to make SimpleNameExpression")
    }
  }
}