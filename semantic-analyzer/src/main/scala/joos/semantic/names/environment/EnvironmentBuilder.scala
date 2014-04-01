package joos.semantic.names.environment

import joos.ast._
import joos.ast.declarations._
import joos.ast.expressions.{SimpleNameExpression, VariableDeclarationExpression}
import joos.ast.statements.WhileStatement
import joos.ast.statements._
import joos.ast.visitor.AstVisitor
import joos.semantic.BlockEnvironment
import scala.collection.mutable

/**
 * Environment builder is responsible for the following name resolution checks:
 *
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

    typed.fields foreach {
      field =>
        if (typed.fieldMap.contains(field.declarationName)) {
          throw new DuplicatedFieldException(field.declarationName)
        }
        typed.add(field)
        field.typeDeclaration = typed
    }
    typed.methods.foreach(_.accept(this))
  }

  override def apply(method: MethodDeclaration) {
    method.blockEnvironment = method.parameters.foldRight(BlockEnvironment()) {
      (variable, environment) =>
        environment.add(variable) match {
          case Some(blockEnvironment) => blockEnvironment
          case None => throw new DuplicatedVariableException(variable.declarationName)
        }
    }
    method.compilationUnit = unit
    method.typeDeclaration = typed
    block = method.blockEnvironment

    method.body.map(_.accept(this))

    // Link in the entire method block
    method.blockEnvironment = block

  }

  override def apply(block: Block) {
//    val oldEnvironment = this.block
    block.blockEnvironment = this.block
    block.statements.foreach(_.accept(this))
    this.block = block.blockEnvironment
  }

  override def apply(statement: IfStatement) {
    statement.blockEnvironment = this.block
    statement.condition.accept(this)
    val oldBlock = block
    statement.trueStatement.accept(this)
    block = oldBlock
    statement.falseStatement.map(_.accept(this))
    block = oldBlock
  }

  override def apply(statement: ExpressionStatement) {
    statement.blockEnvironment = this.block
    statement.expression.accept(this)
  }

  override def apply(statement: WhileStatement) {
    statement.blockEnvironment = this.block
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
    expression.blockEnvironment = block
  }

  override def apply(statement: ForStatement) {
    statement.blockEnvironment = this.block
    val oldBlock = block
    statement.initialization.map(_.accept(this))
    statement.condition.map(_.accept(this))
    statement.update.map(_.accept(this))
    statement.body.accept(this)
    block = oldBlock
  }

  override def apply(statement: ReturnStatement) {
    statement.blockEnvironment = this.block
    val oldBlock = block
    statement.expression.map(_.accept(this))
    block = oldBlock
  }
}
