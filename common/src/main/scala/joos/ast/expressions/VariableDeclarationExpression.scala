package joos.ast.expressions

import joos.ast.declarations.VariableDeclarationFragment
import joos.ast.exceptions.AstConstructionException
import joos.ast.{TypedDeclaration, Modifier, Type}
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

case class VariableDeclarationExpression(
    modifiers: Seq[Modifier],
    variableType: Type,
    declaration: VariableDeclarationFragment) extends Expression with TypedDeclaration {
  override def declarationName = declaration.identifier

  override def declarationType = variableType
}

object VariableDeclarationExpression {
  def apply(ptn: ParseTreeNode): VariableDeclarationExpression = {
    ptn match {
      case TreeNode(ProductionRule("LocalVariableDeclaration", Seq("Type", "VariableDeclarator")), _, children) =>
        VariableDeclarationExpression(Seq.empty, Type(children(0)), VariableDeclarationFragment(children(1)))
      case _ => throw new AstConstructionException("No production rule to create VariableDeclarationExpression")
    }
  }
}
