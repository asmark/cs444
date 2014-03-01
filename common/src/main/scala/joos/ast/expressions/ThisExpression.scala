package joos.ast.expressions

import joos.tokens.{TokenKind, TerminalToken}
import joos.parsetree.{LeafNode, ParseTreeNode}
import joos.ast.exceptions.AstConstructionException

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
