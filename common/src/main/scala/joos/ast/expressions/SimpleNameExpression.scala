package joos.ast.expressions

import joos.ast.exceptions.AstConstructionException
import joos.parsetree.{TreeNode, LeafNode, ParseTreeNode}
import joos.tokens.{TokenKind, TerminalToken}
import joos.language.ProductionRule

case class SimpleNameExpression(identifier: TerminalToken) extends NameExpression {
  def standardName = identifier.lexeme
}

object SimpleNameExpression {
  def apply(ptn: ParseTreeNode): SimpleNameExpression = {
    ptn match {
      case LeafNode(token) if token.kind == TokenKind.Id =>
        return SimpleNameExpression(token)
      case TreeNode(ProductionRule("SimpleName", Seq("Identifier")), _, children) =>
        return SimpleNameExpression(children(0))
      case _ => throw new AstConstructionException("No valid production rule to make SimpleNameExpression")
    }
  }

  def apply(name: String): SimpleNameExpression = {
    SimpleNameExpression(TerminalToken(name, TokenKind.Id))
  }
}
