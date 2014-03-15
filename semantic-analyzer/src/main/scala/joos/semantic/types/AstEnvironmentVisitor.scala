package joos.semantic.types

import joos.ast.CompilationUnit
import joos.ast.declarations.MethodDeclaration
import joos.ast.expressions._
import joos.ast.statements._
import joos.ast.types.Type
import joos.ast.visitor.AstCompleteVisitor
import joos.semantic.types.disambiguation._
import joos.semantic.types.disambiguation.Visibility._
import joos.semantic.{BlockEnvironment, TypeEnvironment}
import scala.Some

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

  protected def resolveFieldAccess(name: NameExpression):Visibility = {
    var visibility = Local

    var names = name match {
      case s:SimpleNameExpression => Seq(s)
      case q:QualifiedNameExpression => q.unfold
    }
    var typeIndex = 1
    var declarationType: Type = null


    // (1) Check local variable
    require(blockEnvironment != null)
    blockEnvironment.getLocalVariable(names.head) match {
      case Some(localVariable) => declarationType = localVariable.declarationType
      case None =>

        // (2) Check local field
        visibility = Local
        getFieldTypeFromType(unit.typeDeclaration.get.asType, names.head, visibility) match {
          case Some(fieldType) => {
            visibility = Local
            declarationType = fieldType
          }
          case None => {

            // (3) Check static accesses
            // Must have a prefix that is a valid type
            while (unit.getVisibleType(names.take(typeIndex)).isEmpty) {
              typeIndex += 1
              if (typeIndex > names.length) {
                throw new AmbiguousNameException(name)
              }
            }

            val typeName = unit.getVisibleType(names.take(typeIndex)).get
            declarationType = typeName.asType

            // Next name must be a static field
            visibility = Static
            if (names.size > typeIndex) {
              val fieldName = names(typeIndex)

              getFieldTypeFromType(declarationType, fieldName, visibility) match {
                case Some(fieldType) => {
                  visibility = Local
                  declarationType = fieldType
                  typeIndex += 1
                }
                case None => throw new AmbiguousNameException(name)
              }
            }

          }
        }
    }
    // All remaining names must be instance field accesses
    names = names.drop(typeIndex)
    names foreach {
      name =>
          visibility
        getFieldTypeFromType(declarationType, name, visibility) match {
          case Some(fieldType) => {
            visibility = Local
            declarationType = fieldType
          }
          case None => throw new AmbiguousNameException(name)
        }
    }
    name.declarationType = declarationType
    visibility
  }
}