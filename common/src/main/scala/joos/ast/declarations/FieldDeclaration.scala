package joos.ast.declarations

import joos.ast.{Type, Modifier}
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException

case class FieldDeclaration(
   modifiers: Seq[Modifier],
   variableType: Type,
   fragment: VariableDeclarationFragment
 ) extends BodyDeclaration

object FieldDeclaration {
  def apply(ptn: ParseTreeNode): FieldDeclaration = {
    ptn match {
      case TreeNode(ProductionRule("FieldDeclaration", Seq("Modifiers", "Type", "VariableDeclarator", ";")),_, children)
        => return new FieldDeclaration(
          Modifier(children(0)),
          Type(children(1)),
          VariableDeclaration(children(2))
        )
      case _ => throw new AstConstructionException(
        "Invalid tree node to create FieldDeclaration"
      )
    }
  }
}