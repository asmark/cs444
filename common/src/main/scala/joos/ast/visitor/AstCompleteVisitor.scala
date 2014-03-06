package joos.ast.visitor

import joos.ast._
import joos.ast.declarations._
import joos.ast.expressions._
import joos.ast.statements._

abstract class AstCompleteVisitor extends AstVisitor {


  override def apply(field: FieldDeclaration) {
    field.fragment.accept(this)
  }

  override def apply(method: MethodDeclaration) {
    method.body foreach (_.accept(this))
    method.parameters foreach (_.accept(this))
  }

  override def apply(variable: SingleVariableDeclaration) {
    variable.initializer foreach (_.accept(this))
  }

  override def apply(typed: TypeDeclaration) {
    typed.fields foreach (_.accept(this))
    typed.methods foreach (_.accept(this))
  }

  override def apply(expression: ArrayAccessExpression) {
    expression.reference.accept(this)
    expression.reference.accept(this)
  }

  override def apply(expression: ArrayCreationExpression) {
    expression.size.accept(this)
  }

  override def apply(expression: AssignmentExpression) {
    expression.left.accept(this)
    expression.right.accept(this)
  }

  override def apply(expression: CastExpression) {
    expression.expression.accept(this)
  }

  override def apply(expression: ClassCreationExpression) {
    expression.arguments foreach (_.accept(this))
  }

  override def apply(expression: FieldAccessExpression) {
    expression.expression.accept(this)
  }

  override def apply(expression: InfixExpression) {
    expression.left.accept(this)
    expression.right.accept(this)
  }

  override def apply(expression: InstanceOfExpression) {
    expression.expression.accept(this)
  }

  override def apply(expression: MethodInvocationExpression) {
    expression.expression foreach (_.accept(this))
    expression.arguments foreach (_.accept(this))
  }

  override def apply(expression: ParenthesizedExpression) {
    expression.expression.accept(this)
  }

  override def apply(expression: PrefixExpression) {
    expression.operand.accept(this)
  }

  override def apply(expression: VariableDeclarationExpression) {
    expression.declaration.accept(this)
  }

  override def apply(expression: VariableDeclarationFragment) {
    expression.initializer foreach (_.accept(this))
  }


  override def apply(expression: ExpressionStatement) {
    expression.expression.accept(this)
  }

  override def apply(statement: ForStatement) {
    statement.initialization foreach (_.accept(this))
    statement.condition foreach (_.accept(this))
    statement.update foreach (_.accept(this))
    statement.body.accept(this)
  }

  override def apply(statement: IfStatement) {
    statement.condition.accept(this)
    statement.trueStatement.accept(this)
    statement.falseStatement foreach (_.accept(this))
  }

  override def apply(statement: ReturnStatement) {
    statement.expression foreach (_.accept(this))
  }

  override def apply(statement: WhileStatement) {
    statement.condition.accept(this)
    statement.body.accept(this)
  }

  override def apply(unit: CompilationUnit) {
    unit.packageDeclaration.accept(this)
    unit.importDeclarations foreach (_.accept(this))
    unit.typeDeclaration foreach (_.accept(this))
  }


  override def apply(block: Block) {
    block.statements foreach (_.accept(this))
  }

}
