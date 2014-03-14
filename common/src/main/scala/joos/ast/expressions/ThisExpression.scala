package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.syntax.parsetree.{LeafNode, ParseTreeNode}
import joos.syntax.tokens.TokenKind

class ThisExpression extends Expression {
  override def toString = "this"
}

object ThisExpression extends Expression {
  private[this] final val instance = new ThisExpression

  def apply(ptn: ParseTreeNode): ThisExpression = {
    ptn match {
      case LeafNode(token) if token.kind == TokenKind.This => instance
      case _ => throw new AstConstructionException("No valid production rule to make ThisExpression")
    }
  }
}
