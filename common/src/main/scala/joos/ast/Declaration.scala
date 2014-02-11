package joos.ast


trait Declaration extends AstNode

case class ImportDeclaration(name: NameExpression, isOnDemand: Boolean) extends Declaration

case class PackageDeclaration(name: NameExpression) extends Declaration

trait VariableDeclaration extends Declaration

case class VariableDeclarationFragment(
  identifier: SimpleNameExpression,
  dimensions: Int,
  initializer: Option[Expression]
)

case class SingleVariableDeclaration(
  modifiers: Seq[Modifier],
  varType: Type,
  identifier: SimpleNameExpression,
  dimensions: Int,
  assignment: Option[Expression]
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
