package joos.semantic.names.environments

import joos.ast._
import joos.ast.declarations._
import joos.ast.expressions.VariableDeclarationExpression
import joos.semantic.{EnvironmentComparisons, BlockEnvironment}

/**
 * EnvironmentBuilder is responsible for the following name resolution checks:
 * No two fields declared in the same class may have the same name.
 * No two local variables with overlapping scope have the same name.
 * No two classes or interfaces have the same canonical name.
 */
class EnvironmentBuilder(implicit module: ModuleDeclaration) extends AstVisitor {
  private[this] implicit var typed: TypeDeclaration = null
  private[this] var unit: CompilationUnit = null
  private[this] var packaged: PackageDeclaration = null
  private[this] var block: BlockEnvironment = null

  override def apply(unit: CompilationUnit) {
    this.unit = unit
    this.packaged = unit.packageDeclaration

    module.add(unit)
    unit.moduleDeclaration = module

    unit.typeDeclaration.map(_.accept(this))
  }

  override def apply(typed: TypeDeclaration) {
    this.typed = typed
    typed.compilationUnit = unit
    typed.packageDeclaration = packaged

    val fieldNames = typed.fields map (_.declarationName)
    EnvironmentComparisons.findDuplicate(fieldNames) map {
      fieldName =>
        throw new DuplicatedFieldException(fieldName)
    }

    typed.methods.foreach(_.accept(this))
  }

  override def apply(method: MethodDeclaration) {
    method.environment = method.parameters.foldRight(BlockEnvironment()) {
      (variable, environment) =>
        environment.add(variable) match {
          case Some(blockEnvironment) => blockEnvironment
          case None => throw new DuplicatedVariableException(variable.declarationName)
        }
    }
    method.typeDeclaration = typed
    block = method.environment

    method.body.map(_.accept(this))
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
    block = block.add(expression) match {
      case Some(blockEnvironment) => blockEnvironment
      case None => throw new DuplicatedVariableException(expression.declarationName)
    }
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
