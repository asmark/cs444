package joos.semantic.types

import joos.semantic.{BlockEnvironment, TypeEnvironment}
import joos.ast.CompilationUnit
import joos.ast.declarations.MethodDeclaration
import joos.ast.statements._
import joos.ast.expressions.{QualifiedNameExpression, VariableDeclarationExpression}
import joos.ast.types.{SimpleType, PrimitiveType, ArrayType, Type}
import joos.semantic.types.disambiguation.{AmbiguousNameException, InvalidStaticUseException}
import joos.ast.visitor.AstCompleteVisitor
import joos.semantic.types.disambiguation._

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

  protected def resolveStaticFieldAccess(name: QualifiedNameExpression) {
    var names = name.unfold
    var typeIndex = 1
    var declarationType: Type = null


    // (1) Check local variable
    require(blockEnvironment != null)
    blockEnvironment.getVariable(names.head) match {
      case Some(localVariable) => declarationType = localVariable.declarationType
      case None =>

        // (2) Check local field
        typeEnvironment.containedFields.get(names.head) match {
          case Some(field) => {
            declarationType = field.declarationType
            if (field.isStatic) {
              throw new InvalidStaticUseException(name)
            }
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

            if (names.size > typeIndex) {
              val fieldName = names(typeIndex)
              typeName.containedFields.get(fieldName) match {
                case Some(field) => {
                  if (!field.isStatic) {
                    throw new InvalidStaticUseException(name)
                  }
                  declarationType = field.variableType
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
        declarationType match {
          // Arrays only have a "length" field
          case _: ArrayType => {
            if (name.standardName equals "length") {
              declarationType = PrimitiveType.BooleanType
            } else {
              throw new AmbiguousNameException(name)
            }
          }
          // Primitives do not have any fields
          case _: PrimitiveType => {
            throw new AmbiguousNameException(name)
          }

          case simpleType: SimpleType => {
            simpleType.declaration.get.containedFields.get(name) match {
              case None => throw new AmbiguousNameException(name)
              case Some(field) => {
                if (field.isStatic) {
                  throw new InvalidStaticUseException(name)
                }
                declarationType = field.variableType
              }
            }
          }
          case _ => throw new AmbiguousNameException(name)
        }
    }
    name.declarationType = declarationType
  }
}