package joos.analyzers

import joos.ast._
import joos.ast.declarations._
import joos.ast.expressions.VariableDeclarationExpression
import joos.semantic.BlockEnvironment

class EnvironmentLinker(implicit module: ModuleDeclaration) extends AstVisitor {
  private[this] implicit var typed: TypeDeclaration = null
  private[this] var unit: CompilationUnit = null
  private[this] var packaged: PackageDeclaration = null
  private[this] var method: MethodDeclaration = null
  private[this] var block: BlockEnvironment = null

  override def apply(unit: CompilationUnit) {
    this.unit = unit
    packaged = unit.packageDeclaration
    module.add(unit)
    unit.typeDeclaration.map(_.accept(this))
  }

  override def apply(typed: TypeDeclaration) {
    this.typed = typed
    typed.compilationUnit = unit
    typed.packageDeclaration = packaged
    typed.methods.foreach(_.accept(this))
    typed.fields.foreach(_.accept(this))
  }

  override def apply(method: MethodDeclaration) {
    this.method = method
    method.compilationUnit = unit
    method.environment = method.parameters.foldRight(BlockEnvironment()) {
      (variable, environment) => environment.add(variable)
    }
    method.typeDeclaration = typed
    block = method.environment
    typed.add(method)
    method.body.map(_.accept(this))
  }

  override def apply(field: FieldDeclaration) {
    typed.add(field)
  }

  override def apply(block: Block) {
    val oldEnvironment = this.block
    block.statements.foreach(_.accept(this))
    this.block = oldEnvironment
  }

  override def apply(statement: IfStatement) {
    statement.condition.accept(this)
    val oldBlock = block
    statement.trueStatement.accept(this)
    block = oldBlock
    statement.falseStatement.map(_.accept(this))
    block = oldBlock
  }

  override def apply(statement: ExpressionStatement) {
    statement.expression.accept(this)
  }

  override def apply(statement: WhileStatement) {
    val oldBlock = block
    statement.condition.accept(this)
    statement.body.accept(this)
    block = oldBlock
  }

  override def apply(expression: VariableDeclarationExpression) {
    block = block.add(expression)
  }

  override def apply(statement: ForStatement) {
    val oldBlock = block
    statement.initialization.map(_.accept(this))
    statement.condition.map(_.accept(this))
    statement.update.map(_.accept(this))
    statement.body.accept(this)
    block = oldBlock
  }
}
