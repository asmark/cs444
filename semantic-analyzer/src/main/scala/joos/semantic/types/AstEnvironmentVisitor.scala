package joos.semantic.types

import joos.semantic.{BlockEnvironment, TypeEnvironment}
import joos.ast.CompilationUnit
import joos.ast.declarations.MethodDeclaration
import joos.ast.statements._
import joos.ast.expressions.{SimpleNameExpression, Expression, QualifiedNameExpression, VariableDeclarationExpression}
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

  protected def resolveMethodAccess(base: Type, methodAccess: QualifiedNameExpression) {
    require(base.declaration.get != null)
    val unfolded = methodAccess.unfold
    var (names, method) = (unfolded.dropRight(1), unfolded.tail)

    var declarationType = base.declaration.get
    names foreach {
      name =>
        declarationType.containedFields.get(name) match {
          case None => throw new AmbiguousNameException(name)
          case Some(field) => declarationType = field.declarationType.declaration.get
        }
    }
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
//        typeEnvironment.containedFields.get(names.head) match {
        getFieldFromType(unit.typeDeclaration.get.asType, names.head) match {
          case Some(fieldType) => {
            // TODO: Static checks
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

            if (names.size > typeIndex) {
              val fieldName = names(typeIndex)

              getFieldFromType(declarationType, fieldName) match {
                case Some(fieldType) => {
                  // TODO: Static checks
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
        getFieldFromType(declarationType, name) match {
          case Some(fieldType) => {
            declarationType = fieldType
          }
          case None => throw new AmbiguousNameException(name)
        }
    }
    name.declarationType = declarationType
  }
}