package joos.ast

import joos.language.ProductionRule
import joos.parsetree.{LeafNode, TreeNode, ParseTreeNode}
import joos.tokens.{TokenKind, TerminalToken}

case class Modifier(modifier: TerminalToken) extends AstNode {
  override def toString = modifier.lexeme
}

object Modifier {
  def apply(ptn: ParseTreeNode): Seq[Modifier] = {
    ptn match {
      case TreeNode(ProductionRule("Modifiers", Seq("Modifiers", "Modifier")), _, children) =>
        return Modifier(children(0)) ++ Modifier(children(1))
      case TreeNode(ProductionRule("Modifiers", Seq("Modifier")), _, children) =>
        return Modifier(children(0))
      case TreeNode(ProductionRule("Modifier", Seq(_)), _, Seq(LeafNode(token))) =>
        return Seq(Modifier(token))
      case _ => throw new AstConstructionException("Invalid production rule to create modifier")
    }
  }

  val Protected = Modifier(TerminalToken("protected", TokenKind.Protected))
  val Public = Modifier(TerminalToken("public", TokenKind.Public))
  val Abstract = Modifier(TerminalToken("abstract", TokenKind.Abstract))
  val Static = Modifier(TerminalToken("static", TokenKind.Static))
  val Final = Modifier(TerminalToken("final", TokenKind.Final))

}
