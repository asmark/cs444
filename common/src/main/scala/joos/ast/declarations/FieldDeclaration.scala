package joos.ast.declarations

import joos.ast.exceptions.AstConstructionException
import joos.ast.{TypedDeclaration, Type, Modifier}
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

case class FieldDeclaration(
    modifiers: Seq[Modifier],
    variableType: Type,
    fragment: VariableDeclarationFragment) extends BodyDeclaration with TypedDeclaration {
  def declarationType = variableType

  def declarationName = fragment.identifier
}

object FieldDeclaration {
  def apply(ptn: ParseTreeNode): FieldDeclaration = {
    ptn match {
      case TreeNode(ProductionRule("FieldDeclaration", Seq("Modifiers", "Type", "VariableDeclarator", ";")), _, children) =>
        FieldDeclaration(Modifier(children(0)), Type(children(1)), VariableDeclarationFragment(children(2)))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create FieldDeclaration"
      )
    }
  }
}
