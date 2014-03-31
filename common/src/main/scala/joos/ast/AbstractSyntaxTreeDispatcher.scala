package joos.ast

import joos.ast.declarations._
import joos.ast.statements._

trait AbstractSyntaxTreeDispatcher extends StatementDispatcher with ExpressionDispatcher {

  def apply(field: FieldDeclaration) {}

  def apply(method: MethodDeclaration) {}

  def apply(variable: SingleVariableDeclaration) {}

  override def apply(statement: ExpressionStatement) {
    this(statement.expression)
  }

  override def apply(statement: ForStatement) {
    statement.initialization.foreach(apply)
    statement.condition.foreach(apply)
    statement.update.foreach(apply)
    this(statement.body)
  }

  override def apply(statement: IfStatement) {
    this(statement.condition)
    this(statement.trueStatement)
    statement.falseStatement.foreach(apply)
  }

  override def apply(statement: ReturnStatement) {
    statement.expression.foreach(apply)
  }

  override def apply(statement: WhileStatement) {
    this(statement.condition)
    this(statement.body)
  }

  def apply(tipe: TypeDeclaration) {
    tipe.fields.foreach(apply)
    tipe.methods.foreach(apply)
  }

  def apply(unit: CompilationUnit) {
    unit.typeDeclaration.foreach(apply)
  }
}
