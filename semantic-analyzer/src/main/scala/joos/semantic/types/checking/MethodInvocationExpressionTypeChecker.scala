package joos.semantic.types.checking

import joos.ast.declarations.MethodDeclaration
import joos.ast.expressions._
import joos.ast.types.Type
import joos.ast.visitor.AstVisitor
import joos.ast.{Modifier, CompilationUnit}
import joos.semantic.types.IllegalProtectedAccessException
import joos.semantic.types.disambiguation.Visibility._
import joos.semantic.types.disambiguation._

trait MethodInvocationExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  private def checkSimpleAccess(method: MethodDeclaration)(implicit unit: CompilationUnit) {
    if (method.modifiers contains Modifier.Protected) {
      val selfType = unit.typeDeclaration.get
      if (!(selfType.packageDeclaration.declarationName equals method.typeDeclaration.packageDeclaration.declarationName)) {
        if (!(selfType.allAncestors contains method.typeDeclaration)) {
          throw new IllegalProtectedAccessException(method.declarationName)
        }
      }
    }
  }

  private def checkQualifiedAccess(method: MethodDeclaration, prefixType: Type)(implicit unit: CompilationUnit) {
      checkSimpleAccess(method)
    if ((method.modifiers contains Modifier.Protected) && (!(method.modifiers contains Modifier.Static))) {
      val selfType = unit.typeDeclaration.get
      if (!(prefixType.declaration.get equals selfType)) {
        if (!(prefixType.declaration.get.allAncestors contains selfType)) {
          throw new IllegalProtectedAccessException(method.declarationName)
        }
      }
    }
  }

  private def getQualifiedMethod(methodAccess: QualifiedNameExpression, parameters: IndexedSeq[Expression]) {

    val unfolded = methodAccess.unfold

    val (fieldPrefix, methodName) = (fold(unfolded.dropRight(1)), unfolded.last)

    val visibility = resolveFieldAccess(fieldPrefix)

    getMethodFromType(fieldPrefix.declarationType, methodName, parameters) match {
      case Some(methodDeclaration) => {
        checkVisibility(methodDeclaration, visibility)
        checkQualifiedAccess(methodDeclaration, fieldPrefix.declarationType)
        methodAccess.declarationType = methodDeclaration.returnType.get
      }
      case None => throw new AmbiguousNameException(methodAccess)
    }

  }

  // Link a method that is called as an isolated expression
  private def linkMethod(methodName: NameExpression, parameters: IndexedSeq[Expression]) {
    methodName match {
      // Must be a local method declaration
      case methodName: SimpleNameExpression => {
        if (inStaticMethod) throw new InvalidStaticUseException(methodName)

        getMethodFromType(unit.typeDeclaration.get.asType, methodName, parameters) match {
          case None => throw new AmbiguousNameException(methodName)
          case Some(method) => {
            checkVisibility(method, Local)
            checkSimpleAccess(method)
            methodName.declarationType = method.returnType.get
          }
        }

      }
      case methodName: QualifiedNameExpression => getQualifiedMethod(methodName, parameters)
    }
  }

  private def linkMethod(left: Type, methodName: NameExpression, parameters: IndexedSeq[Expression]) {
    methodName match {
      case methodName: SimpleNameExpression => {

        getMethodFromType(left, methodName, parameters) match {
          case None => throw new AmbiguousNameException(methodName)
          case Some(method) => {
            checkVisibility(method, Local)
            checkQualifiedAccess(method, left)
            methodName.declarationType = method.returnType.get
          }
        }
      }

      case methodName: QualifiedNameExpression => {
        var leftType = left
        methodName.unfold.dropRight(1) foreach {
          name =>
            getFieldTypeFromType(leftType, name, Local) match {
              case None => throw new AmbiguousNameException(methodName)
              case Some(returnType) => {
                leftType = returnType
              }
            }

            getMethodFromType(leftType, methodName.unfold.last, parameters) match {
              case None => throw new AmbiguousNameException(methodName)
              case Some(method) => {
                checkVisibility(method, Local)
                checkQualifiedAccess(method, leftType)
                methodName.declarationType = method.returnType.get
              }
            }
        }
      }
    }
  }

  override def apply(invocation: MethodInvocationExpression) {
    invocation.arguments.foreach(
      expr => {
        expr.accept(this)
        require(expr.declarationType != null)
      }
    )
    invocation.expression.foreach(
      expr => {
        expr.accept(this)
        require(expr.declarationType != null)
      }
    )

    invocation.expression match {
      case None => linkMethod(invocation.methodName, invocation.arguments)
      case Some(expression) => linkMethod(expression.declarationType, invocation.methodName, invocation.arguments)
    }
    invocation.declarationType = invocation.methodName.declarationType
  }


}
