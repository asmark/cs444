package joos.ast

import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.ast.exceptions.AstConstructionException


trait Declaration extends AstNode

case class ImportDeclaration(name: NameExpression, isOnDemand: Boolean) extends Declaration

object ImportDeclaration {
  def apply(ptn: ParseTreeNode) : Seq[ImportDeclaration] = {
    ptn match {
      case TreeNode(
      ProductionRule("ImportDeclarations", Seq("ImportDeclarations", "ImportDeclaration")),
      _,
      children
      ) =>
        return ImportDeclaration(children(0)) ++ ImportDeclaration(children(1))
      case TreeNode(ProductionRule("ImportDeclaration", _), _, children) =>
        return ImportDeclaration(children(0))
      case TreeNode(ProductionRule("SingleTypeImportDeclaration", _), _, children) =>
        return Seq(ImportDeclaration(NameExpression(children(1)), false))
      case TreeNode(ProductionRule("TypeImportOnDemandDeclaration", _), _, children) =>
        return Seq(ImportDeclaration(NameExpression(children(1))), true)
      case _ => throw new AstConstructionException("No valid production rule to create ImportDeclaration")
    }
  }
}

case class PackageDeclaration(name: NameExpression) extends Declaration

trait VariableDeclaration extends Declaration {
  def identifier: SimpleNameExpression
  def initializer: Option[Expression]
}

case class VariableDeclarationFragment(
  identifier: SimpleNameExpression,
  initializer: Option[Expression]
)

case class SingleVariableDeclaration(
  modifiers: Seq[Modifier],
  varType: Type,
  identifier: SimpleNameExpression,
  initializer: Option[Expression]
) extends VariableDeclaration

trait BodyDeclaration extends AstNode {
  val modifiers: Seq[Modifier]
}

case class MethodDeclaration(
  modifiers: Seq[Modifier],
  retType: Option[Type],
  retDims: Int,
  name: SimpleNameExpression,
  params: Seq[SingleVariableDeclaration],
  body: Block,
  isConstructor: Boolean
) extends BodyDeclaration

case class FieldDeclaration(
  modifiers: Seq[Modifier],
  retType: Type,
  fragment: VariableDeclarationFragment
) extends BodyDeclaration

case class TypeDeclaration(
  modifiers: Seq[Modifier],
  isInterface: Boolean,
  superType: NameExpression,
  superInterfaces: Seq[NameExpression],
  fields: Seq[FieldDeclaration],
  methods: Seq[MethodDeclaration]
) extends BodyDeclaration
