package joos.semantic.types.checking

import joos.ast.expressions._
import joos.ast.visitor.AstVisitor
import joos.semantic.types.disambiguation._
import joos.ast.types.Type
import joos.ast.Modifier
import joos.semantic.types.IllegalProtectedMethodAccessException

trait MethodInvocationExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  private def getStaticAccessMethod(methodAccess: QualifiedNameExpression, parameters: IndexedSeq[Expression]) {

    val unfolded = methodAccess.unfold

    val (fieldPrefix, methodName) = (fold(unfolded.dropRight(1)), unfolded.last)

    resolveStaticFieldAccess(fieldPrefix)

    getMethodFromType(fieldPrefix.declarationType, methodName, parameters) match {
      case Some(method) => {
        if (method.modifiers.contains(Modifier.Protected) && method.typeDeclaration != unit.typeDeclaration.get) {
          if (!unit.typeDeclaration.get.allAncestors.contains(method.typeDeclaration) &&
              unit.packageDeclaration != method.typeDeclaration.packageDeclaration) {
            throw new IllegalProtectedMethodAccessException(
              s"${fieldPrefix.toString} pointing to ${method.declarationName.standardName}"
            )
          }
        }
        method.returnType match {
          case Some(returnType) => methodAccess.declarationType = returnType
          case None => throw new AmbiguousNameException(methodAccess)
        }
      }
      case _ => // TODO: exception?
    }
  }

  private def linkMethod(left: Type, methodName: NameExpression, parameters: IndexedSeq[Expression]) {
    methodName match {
      case methodName: SimpleNameExpression => {
        getMethodTypeFromType(left, methodName, parameters) match {
          case None => throw new AmbiguousNameException(methodName)
          case Some(returnType) => methodName.declarationType = returnType
        }
      }

      case methodName: QualifiedNameExpression => {
        var leftType = left
        methodName.unfold.dropRight(1) foreach {
          name =>
            getFieldFromType(leftType, name) match {
              case None => throw new AmbiguousNameException(methodName)
              case Some(returnType) => {
                leftType = returnType
              }
            }
            getMethodTypeFromType(leftType, methodName.unfold.last, parameters) match {
              case Some(returnType) => methodName.declarationType = returnType
              case None => throw new AmbiguousNameException(methodName)
            }
        }
      }
    }
  }

  // Link a method that is called as an isolated expression
  private def linkMethod(methodName: NameExpression, parameters: IndexedSeq[Expression]) {
    methodName match {
      // Must be a local method declaration
      case methodName: SimpleNameExpression => {
        getMethodTypeFromType(unit.typeDeclaration.get.asType, methodName, parameters) match {
          case None => throw new AmbiguousNameException(methodName)
          case Some(returnType) => methodName.declarationType = returnType
        }
      }
      case methodName: QualifiedNameExpression => getStaticAccessMethod(methodName, parameters)
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
