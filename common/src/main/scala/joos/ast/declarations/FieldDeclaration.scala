package joos.ast.declarations

import joos.ast.AstConstructionException
import joos.ast.Modifier
import joos.ast.compositions.TypedDeclarationLike
import joos.ast.types.Type
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}
import joos.core.Identifiable

case class FieldDeclaration(
    modifiers: Seq[Modifier],
    variableType: Type,
    fragment: VariableDeclarationFragment) extends BodyDeclaration with TypedDeclarationLike with Identifiable {
  def declarationType = variableType

  def declarationName = fragment.identifier

  var typeDeclaration: TypeDeclaration = null

  lazy val isStatic = {
    require(typeDeclaration != null)
    typeDeclaration.isInterface || (modifiers contains Modifier.Static)
  }
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
