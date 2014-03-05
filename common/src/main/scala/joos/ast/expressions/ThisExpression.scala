package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.syntax.parsetree.{LeafNode, ParseTreeNode}
import joos.syntax.tokens.{TokenKind, TerminalToken}

case class ThisExpression(token: TerminalToken) extends Expression

object ThisExpression {
  def apply(ptn: ParseTreeNode): ThisExpression = {
    ptn match {
      case LeafNode(token) if token.kind == TokenKind.This =>
        ThisExpression(token)
      case _ => throw new AstConstructionException("No valid production rule to make ThisExpression")
    }
  }
}
