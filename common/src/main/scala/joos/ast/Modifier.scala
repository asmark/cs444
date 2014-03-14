package joos.ast

import joos.core.Enumeration
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{LeafNode, TreeNode, ParseTreeNode}

class Modifier(val name: String) extends AstNode with Modifier.Value

object Modifier extends Enumeration {
  type T = Modifier

  final val Protected = this + new Modifier("protected")
  final val Public = this + new Modifier("public")
  final val Abstract = this + new Modifier("abstract")
  final val Static = this + new Modifier("static")
  final val Final = this + new Modifier("final")
  final val Native = this + new Modifier("native")

  def apply(ptn: ParseTreeNode): Seq[Modifier] = {
    ptn match {
      case TreeNode(ProductionRule("Modifiers", Seq("Modifiers", "Modifier")), _, children) =>
        Modifier(children(0)) ++ Modifier(children(1))
      case TreeNode(ProductionRule("Modifiers", Seq("Modifier")), _, children) =>
        Modifier(children(0))
      case TreeNode(ProductionRule("Modifier", Seq(_)), _, Seq(LeafNode(token))) =>
        Seq(fromName(token.lexeme))
      case _ => throw new AstConstructionException("Invalid production rule to create modifier")
    }
  }
}
