package joos.ast.visitor

import joos.ast._
import joos.ast.declarations._
import joos.ast.expressions._
import joos.ast.statements._

trait AstVisitor {
  def apply(field: FieldDeclaration) {}
  def apply(importd: ImportDeclaration) {}
  def apply(method: MethodDeclaration) {}
//  def apply(module: ModuleDeclaration) {}
  def apply(packaged: PackageDeclaration) {}
  def apply(variable: SingleVariableDeclaration) {}
  def apply(typed: TypeDeclaration) {}
  def apply(expression: ArrayAccessExpression) {}
  def apply(expression: ArrayCreationExpression) {}
  def apply(expression: AssignmentExpression) {}
//  def apply(expression: BooleanLiteral) {}
  def apply(expression: CastExpression) {}
//  def apply(expression: CharacterLiteral) {}
  def apply(expression: ClassInstanceCreationExpression) {}
  def apply(expression: FieldAccessExpression) {}
  def apply(expression: InfixExpression) {}
  def apply(expression: InstanceOfExpression) {}
//  def apply(expression: IntegerLiteral) {}
  def apply(expression: MethodInvocationExpression) {}
//  def apply(expression: NullLiteral) {}
  def apply(expression: ParenthesizedExpression) {}
  def apply(expression: PrefixExpression) {}
  def apply(expression: QualifiedNameExpression) {}
  def apply(expression: SimpleNameExpression) {}
//  def apply(expression: StringLiteral) {}
  def apply(expression: ThisExpression) {}
  def apply(expression: VariableDeclarationExpression) {}
  def apply(expression: VariableDeclarationFragment) {}
  def apply(statement: ExpressionStatement) {}
  def apply(statement: ForStatement) {}
  def apply(statement: IfStatement) {}
  def apply(statement: ReturnStatement) {}
  def apply(statement: WhileStatement) {}
  def apply(compilationUnit: CompilationUnit) {}
  def apply(node: Block) {}
}
