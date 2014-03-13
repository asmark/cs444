package joos.semantic.types.disambiguation

import joos.ast.CompilationUnit
import joos.ast.compositions.LikeName._
import joos.ast.declarations.MethodDeclaration
import joos.ast.expressions._
import joos.ast.statements._
import joos.ast.types.{PrimitiveType, ArrayType, SimpleType, Type}
import joos.ast.visitor.AstCompleteVisitor
import joos.core.Logger
import joos.semantic.{BlockEnvironment, TypeEnvironment}
import joos.syntax.tokens.TerminalToken
import joos.syntax.tokens.TokenKind
import scala.Some

class StaticAndVariableNameLinker(implicit unit: CompilationUnit) extends AstCompleteVisitor {
  private var typeEnvironment: TypeEnvironment = null
  private var blockEnvironment: BlockEnvironment = null

  private def fullType(typeName: Type, unit: CompilationUnit): Type = {
    typeName match {
      case PrimitiveType(t) => PrimitiveType(t)
      case ArrayType(t, dims) => ArrayType(fullType(t, unit), dims)
      case SimpleType(t) => {
        val typeDeclaration = unit.getVisibleType(t).get
        SimpleType(QualifiedNameExpression(typeDeclaration.packageDeclaration.name, typeDeclaration.name))
      }
    }
  }
  
  private def getMethod(typeName: Type, methodName: SimpleNameExpression): Option[Type] = {
    typeName match {
      case SimpleType(t) => {
        unit.getVisibleType(t).get.containedMethods.get(methodName) match {
          case None => None
          case Some(methodDeclarations) => {
            methodDeclarations.head.returnType match {
              case Some(returnType) => Some(fullType(returnType, methodDeclarations.head.typeDeclaration.compilationUnit))
              case None => {
                if (methodDeclarations.head.isConstructor) {
                  throw new AmbiguousNameException(methodName)
                } else {
                  // TODO: Convert
                  Some(PrimitiveType(TerminalToken("void", TokenKind.Void)))
                  //simpleName.declarationType = void
                }
              }
            }
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
            Some(fullType(field.variableType, field.typeDeclaration.compilationUnit))
          }
        }
      }
      case ArrayType(t, dim) => {
        fieldName.standardName match {
          // TODO: Convert
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

  override def apply(invocation: MethodInvocationExpression) {
    invocation.expression foreach (_.accept(this))
    invocation.arguments foreach (_.accept(this))
    invocation.methodName.accept(this)
  }

  override def apply(expression: ArrayAccessExpression) {
    expression.reference.accept(this)
    expression.index.accept(this)
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
          case q: SimpleNameExpression => resolve(q)
        }
        val qualifier = qualifiedName.qualifier
        assert(qualifier.declarationType != null)

        val simpleName = qualifiedName.name
        // Check if simpleType is a method on q. If so, set the declaration to be the return type declaration
        if (getField(qualifier.declarationType, simpleName).isDefined) {
          qualifiedName.declarationType = getField(qualifier.declarationType, simpleName).get
        } else if (getMethod(qualifier.declarationType, simpleName).isDefined) {
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

  def resolve(simpleName: SimpleNameExpression) {
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
        } else {
          typeEnvironment.containedFields.get(simpleName) match {
            case None => throw new AmbiguousNameException(simpleName)
            case Some(fieldDeclaration) => {
              simpleName.declarationType = fieldDeclaration.variableType
            }
          }
        }
      }
      case MethodName => {
        typeEnvironment.containedMethods.get(simpleName) match {
          case None => {
            throw new AmbiguousNameException(simpleName)
          }
          case Some(methodDeclarations) => {
            methodDeclarations.head.returnType match {
              case Some(returnType) => simpleName.declarationType = returnType
              case None => {
                if (methodDeclarations.head.isConstructor) {
                  throw new AmbiguousNameException(simpleName)
                } else {
                  // TODO: Convert
                  simpleName.declarationType = PrimitiveType(TerminalToken("void", TokenKind.Void))
                  //simpleName.declarationType = void
                }
              }
            }
          }
        }
      }
    }
  }

}
