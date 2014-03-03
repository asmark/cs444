package joos.ast

import joos.ast.declarations._
import joos.ast.expressions._

trait AstVisitor {
  def apply(field: FieldDeclaration) {}
  def apply(importd: ImportDeclaration) {}
  def apply(method: MethodDeclaration) {}
  def apply(module: ModuleDeclaration) {}
  def apply(packaged: PackageDeclaration) {}
  def apply(variable: SingleVariableDeclaration) {}
  def apply(typed: TypeDeclaration) {}
  def apply(expression: ArrayAccessExpression) {}
  def apply(expression: ArrayCreationExpression) {}
  def apply(expression: AssignmentExpression) {}
  def apply(expression: BooleanLiteral) {}
  def apply(expression: CastExpression) {}
  def apply(expression: CharacterLiteral) {}
  def apply(expression: ClassCreationExpression) {}
  def apply(expression: FieldAccessExpression) {}
  def apply(expression: InfixExpression) {}
  def apply(expression: InstanceOfExpression) {}
  def apply(expression: IntegerLiteral) {}
  def apply(expression: MethodInvocationExpression) {}
  def apply(expression: NullLiteral) {}
  def apply(expression: ParenthesizedExpression) {}
  def apply(expression: PrefixExpression) {}
  def apply(expression: StringLiteral) {}
  def apply(expression: ThisExpression) {}
  def apply(expression: VariableDeclarationExpression) {}
  def apply(statement: ExpressionStatement) {}
  def apply(statement: ForStatement) {}
  def apply(statement: IfStatement) {}
  def apply(statement: ReturnStatement) {}
  def apply(statement: WhileStatement) {}
  def apply(compilationUnit: CompilationUnit) {}
  def apply(node: Block) {}
}
