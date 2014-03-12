package joos.semantic.types.disambiguation

import joos.ast.CompilationUnit
import joos.ast.compositions.LikeName._
import joos.ast.declarations.{TypeDeclaration, MethodDeclaration}
import joos.ast.expressions.{VariableDeclarationExpression, SimpleNameExpression, QualifiedNameExpression}
import joos.ast.statements._
import joos.ast.types.{PrimitiveType, ArrayType, SimpleType, Type}
import joos.ast.visitor.AstCompleteVisitor
import joos.core.Logger
import joos.semantic.{BlockEnvironment, TypeEnvironment}
import joos.syntax.tokens.{TokenKind, TerminalToken}

class NameLinker(implicit unit: CompilationUnit) extends AstCompleteVisitor {

  private var typeEnvironment: TypeEnvironment = null
  private var blockEnvironment: BlockEnvironment = null

  private def getMethod(typeName: Type, methodName: SimpleNameExpression): Option[Type] = {
    // array length first
    typeName match {
      case SimpleType(t) => {
        unit.getVisibleType(t).get.containedMethods.get(methodName) match {
          case None => None
          case Some(methods) => {
            methods.head.returnType
          }
        }
      }
      case ArrayType(t, dim) => None
      case PrimitiveType(t) => None
    }
  }

  private def getField(typeName: Type, fieldName: SimpleNameExpression): Option[Type] = {
    typeName match {
      case SimpleType(t) => {
        unit.getVisibleType(t).get.containedFields.get(fieldName) match {
          case None => None
          case Some(field) => {
            Some(field.variableType)
          }
        }
      }
      case ArrayType(t, dim) => {
        fieldName.standardName match {
          case "length" => Some(PrimitiveType(TerminalToken("int", TokenKind.Int)))
          case _ => None
        }
      }
      case PrimitiveType(t) => None
    }
  }

  override def apply(unit: CompilationUnit) {
    typeEnvironment = unit.typeDeclaration.getOrElse(null)

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
    expression.declaration.accept(this)
  }

  override def apply(qualifiedName: QualifiedNameExpression) {
    require(qualifiedName.classification != Ambiguous)
    qualifiedName.classification match {
      case TypeName => {
        unit.getVisibleType(qualifiedName) match {
          case None => throw new AmbiguousNameException(qualifiedName)
          case Some(typeDeclaration) => qualifiedName.declarationType = SimpleType(qualifiedName)
        }
      }
      case PackageName => {
        Logger.logError(s"${qualifiedName} was interpreted as a PackageName at Linking time")
        throw new AmbiguousNameException(qualifiedName)
      }
      case PackageOrTypeName => {
        Logger.logError(s"${qualifiedName} was interpreted as a PackageName at Linking time")
        throw new AmbiguousNameException(qualifiedName)
      }
      case ExpressionName => {
        // Resolve left side of name first, and then use it to resolve self
        qualifiedName.qualifier match {
          case q: QualifiedNameExpression => apply(q)
          case q: SimpleNameExpression => apply(q)
        }
        val qualifier = qualifiedName.qualifier
        assert(qualifier.declarationType != null)

        val simpleName = qualifiedName.name
        // Check if simpleType is a method on q. If so, set the declaration to be the return type declaration
        if (getField(qualifier.declarationType, simpleName).isDefined) {
          qualifiedName.declarationType = getField(qualifier.declarationType, simpleName).get
        } else if (getMethod(qualifier.declarationType, simpleName).isDefined) {
          // TODO: Last .get call might fail if reference a constructor
          qualifiedName.declarationType = getMethod(qualifier.declarationType, simpleName).get
        } else {
          throw new AmbiguousNameException(qualifiedName)
        }
      }
      case MethodName => {
        assume(qualifiedName.qualifier.classification == TypeName)

        val qualifier = qualifiedName.qualifier
        unit.getVisibleType(qualifier) match {
          case None => throw new AmbiguousNameException(qualifiedName)
          case Some(typeDeclaration) => {
            val simpleName = qualifiedName.name
            getMethod(SimpleType(typeDeclaration.name), simpleName) match {
              case Some(returnType) => qualifiedName.declarationType = returnType
              case None => throw new AmbiguousNameException(qualifiedName)
            }
          }
        }
      }
    }
  }

  override def apply(simpleName: SimpleNameExpression) {
    require(simpleName.classification != Ambiguous)
    simpleName.classification match {
      case PackageName => Logger.logInformation(s"SimpleName ${simpleName} is a PackageName")
      case PackageOrTypeName => Logger.logInformation(s"SimpleName ${simpleName} is a PackageOrTypeName")
      case TypeName => {
        unit.getVisibleType(simpleName) match {
          case None => throw new AmbiguousNameException(simpleName)
          case Some(typeDeclaration) => {
            simpleName.declarationType = SimpleType(typeDeclaration.name)
          }
        }
      }
      case ExpressionName => {
        if (blockEnvironment != null && blockEnvironment.getVariable(simpleName).isDefined) {
          simpleName.declarationType = blockEnvironment.getVariable(simpleName).get.declarationType
          //              getTypeDeclarationFromType(blockEnvironment.getVariable(simpleName).get.declarationType)
        } else {
          typeEnvironment.containedFields.get(simpleName) match {
            case None => throw new AmbiguousNameException(simpleName)
            case Some(fieldDeclaration) => {
              simpleName.declarationType = fieldDeclaration.variableType
              //                  getTypeDeclarationFromType(typeEnvironment.containedFields(simpleName).variableType)
            }
          }
        }
      }
      case MethodName => {
        typeEnvironment.containedMethods.get(simpleName) match {
          case None => throw new AmbiguousNameException(simpleName)
          case Some(methodDeclarations) => {
            // TODO: Last .get call might fail if reference a constructor
            // TODO: Link parameter types to correct method call
            simpleName.declarationType = methodDeclarations.head.returnType.get
          }
        }
      }
    }
  }

}
