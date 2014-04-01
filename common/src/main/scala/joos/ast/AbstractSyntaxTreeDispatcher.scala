package joos.ast

import joos.ast.declarations._
import joos.ast.expressions.Expression
import joos.ast.statements._

trait AbstractSyntaxTreeDispatcher extends StatementDispatcher with ExpressionDispatcher {

  def apply(node: AstNode) {
    node match {
      case expression: Expression => dispatchExpression(expression)
      case statement: Statement => dispatchStatement(statement)
      case field: FieldDeclaration => this(field)
      case method: MethodDeclaration => this(method)
      case tipe: TypeDeclaration => this(tipe)
      case variable: SingleVariableDeclaration => this(variable)
      case fragment: VariableDeclarationFragment => this(fragment)
    }
  }

  def apply(field: FieldDeclaration) {}

  def apply(method: MethodDeclaration) {}

  def apply(variable: SingleVariableDeclaration) {}

  def apply(fragment: VariableDeclarationFragment) {}

  override def apply(statement: ExpressionStatement) {
    dispatchExpression(statement.expression)
  }

  override def apply(statement: ForStatement) {
    statement.initialization.foreach(dispatchExpression)
    statement.condition.foreach(dispatchExpression)
    statement.update.foreach(dispatchExpression)
    dispatchStatement(statement.body)
  }

  override def apply(statement: IfStatement) {
    dispatchExpression(statement.condition)
    dispatchStatement(statement.trueStatement)
    statement.falseStatement.foreach(dispatchStatement)
  }

  override def apply(statement: ReturnStatement) {
    statement.expression.foreach(dispatchExpression)
  }

  override def apply(statement: WhileStatement) {
    dispatchExpression(statement.condition)
    dispatchStatement(statement.body)
  }

  def apply(tipe: TypeDeclaration) {
    tipe.fields.foreach(apply)
    tipe.methods.foreach(apply)
  }

  def apply(unit: CompilationUnit) {
    unit.typeDeclaration.foreach(apply)
  }
}
