package joos.ast.visitor

import joos.semantic.{BlockEnvironment, TypeEnvironment}
import joos.ast.CompilationUnit
import joos.ast.declarations.MethodDeclaration
import joos.ast.statements._
import joos.ast.expressions.VariableDeclarationExpression

class AstEnvironmentVisitor(implicit unit: CompilationUnit) extends AstCompleteVisitor {
  protected var typeEnvironment: TypeEnvironment = null
  protected var blockEnvironment: BlockEnvironment = null

  override def apply(unit: CompilationUnit) {
    typeEnvironment = unit.typeDeclaration.getOrElse(null)
    blockEnvironment = BlockEnvironment()(typeEnvironment)

    super.apply(unit)
  }

  override def apply(methodDeclaration: MethodDeclaration) {
    val oldBlock = blockEnvironment
    blockEnvironment = methodDeclaration.environment
    super.apply(methodDeclaration)
    blockEnvironment = oldBlock
  }

  override def apply(block: Block) {
    val oldBlock = blockEnvironment
    blockEnvironment = block.environment
    block.statements foreach (_.accept(this))
    blockEnvironment = oldBlock
  }

  override def apply(statement: IfStatement) {
    val oldBlock = blockEnvironment
    blockEnvironment = statement.environment
    statement.condition.accept(this)
    blockEnvironment = statement.environment
    statement.trueStatement.accept(this)
    blockEnvironment = statement.environment
    statement.falseStatement.map(_.accept(this))
    blockEnvironment = oldBlock
  }

  override def apply(statement: ExpressionStatement) {
    val oldBlock = blockEnvironment
    blockEnvironment = statement.environment
    statement.expression.accept(this)
    blockEnvironment = oldBlock
  }

  override def apply(statement: WhileStatement) {
    val oldBlock = blockEnvironment
    blockEnvironment = statement.environment
    statement.condition.accept(this)
    statement.body.accept(this)
    blockEnvironment = oldBlock
  }

  override def apply(statement: ForStatement) {
    val oldBlock = blockEnvironment
    blockEnvironment = statement.environment
    statement.initialization.map(_.accept(this))
    statement.condition.map(_.accept(this))
    statement.update.map(_.accept(this))
    statement.body.accept(this)
    blockEnvironment = oldBlock
  }

  override def apply(statement: ReturnStatement) {
    val oldBlock = blockEnvironment
    blockEnvironment = statement.environment
    statement.expression.map(_.accept(this))
    blockEnvironment = oldBlock
  }

  override def apply(expression: VariableDeclarationExpression) {
    blockEnvironment = expression.environment
    expression.fragment.accept(this)
  }
}