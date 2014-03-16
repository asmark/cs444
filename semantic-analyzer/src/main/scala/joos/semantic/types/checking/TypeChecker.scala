package joos.semantic.types.checking

import joos.ast.declarations.{TypeDeclaration, MethodDeclaration, FieldDeclaration}
import joos.ast.statements.{ForStatement, ReturnStatement, WhileStatement, IfStatement}
import joos.ast.types.PrimitiveType._
import joos.ast.types.{Type, PrimitiveType}
import joos.ast.{Modifier, CompilationUnit}
import joos.semantic._
import joos.semantic.types._

class TypeChecker(implicit val unit: CompilationUnit)
    extends AstEnvironmentVisitor
    with AssignmentExpressionTypeChecker
    with ArrayAccessExpressionTypeChecker
    with ArrayCreationExpressionTypeChecker
    with CastExpressionTypeChecker
    with ClassInstanceCreationExpressionTypeChecker
    with FieldAccessExpressionTypeChecker
    with InfixExpressionTypeChecker
    with MethodInvocationExpressionTypeChecker
    with ParenthesizedExpressionTypeChecker
    with ThisExpressionTypeChecker
    with VariableDeclarationExpressionTypeChecker
    with PrefixExpressionTypeChecker
    with InstanceOfExpressionTypeChecker {
  protected var checkExplicitThis = false
  protected var checkMethodReturns: Option[Type] = None
  protected var inStaticMethod = false

  override def apply(fieldDeclaration: FieldDeclaration) {
    // Check that the implicit this variable is not accessed in a static method or in the initializer of a static field.
    checkExplicitThis = fieldDeclaration.modifiers.contains(Modifier.Static)
    super.apply(fieldDeclaration)
    checkExplicitThis = false

    fieldDeclaration.fragment.initializer foreach {
      initializer =>
        if (!isAssignable(fieldDeclaration.variableType, initializer.declarationType))
          throw new FieldDeclarationTypeException(s"${initializer.declarationType} can not be assigned to ${fieldDeclaration.variableType}")
    }
  }

  override def apply(methodDeclaration: MethodDeclaration) {
    checkMethodReturns = methodDeclaration.returnType
    checkExplicitThis = methodDeclaration.modifiers.contains(Modifier.Static)
    inStaticMethod = methodDeclaration.isStatic

    super.apply(methodDeclaration)

    checkExplicitThis = false
    checkMethodReturns = None
    inStaticMethod = false
  }

  override def apply(typeDeclaration: TypeDeclaration) {
    // A constructor in a class other than java.lang.Object implicitly calls the zero-argument constructor of its superclass.
    // Check that this zero-argument constructor exists.
    getSuperType(typeDeclaration) foreach {
      superType => {
        val zeroArgConstructor = superType.constructorMap.values.find(_.parameters.size == 0)
        if (zeroArgConstructor.isEmpty)
          throw new MissingConstructorException(s"Missing zero argument constructor in ${superType.declarationName.standardName}")
      }
    }

    typeDeclaration.constructorMap.values.foreach {
      constructor =>
        if (!(constructor.name equals typeDeclaration.name)) {
          throw new InvalidConstructorException(s"Mismatched constructor and type declaration ${typeDeclaration.declarationName.standardName}")
        }
    }
    super.apply(typeDeclaration)
  }

  override def apply(statement: IfStatement) {
    super.apply(statement)

    require(statement.condition.declarationType != null)
    if (statement.condition.declarationType != PrimitiveType.BooleanType) {
      throw new TypeCheckingException("IfStatement", s"Conditional statement was ${statement.condition.declarationType}. Expected Boolean")
    }
  }

  override def apply(statement: WhileStatement) {
    super.apply(statement)

    require(statement.condition.declarationType != null)
    if (statement.condition.declarationType != PrimitiveType.BooleanType) {
      throw new TypeCheckingException("WhileStatement", s"Conditional statement was ${statement.condition.declarationType}. Expected Boolean")
    }
  }

  override def apply(forStatement: ForStatement) {
    super.apply(forStatement)

    forStatement.condition match {
      case None =>
      case Some(condition) =>
        require(condition.declarationType != null)

        if (condition.declarationType != BooleanType)
          throw new TypeCheckingException("for", s"condition needs to be boolean instead of ${condition.declarationType.standardName}")
    }
  }

  override def apply(statement: ReturnStatement) {
    super.apply(statement)

    if (checkMethodReturns.isEmpty) {
      if (statement.expression.isDefined) {
        throw new TypeCheckingException("ReturnStatement", s"Found a non-empty return statement in a constructor")
      }
    } else {
      val expectedReturnType = checkMethodReturns.get
      statement.expression match {
        case Some(expression) =>
          if (expectedReturnType == PrimitiveType.VoidType || !isAssignable(expectedReturnType, expression.declarationType)) {
            throw new TypeCheckingException(
              "ReturnStatement",
              s"Return statement attempted to return ${expression.declarationType}. Expected ${expectedReturnType}")
          }
        case None => {
          if (expectedReturnType != PrimitiveType.VoidType) {
            throw new TypeCheckingException("ReturnStatement", s"Empty return statement but expected ${expectedReturnType}")
          }
        }
      }
    }
  }
}
