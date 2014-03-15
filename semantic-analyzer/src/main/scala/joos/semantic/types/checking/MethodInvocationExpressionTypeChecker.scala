package joos.semantic.types.checking

import joos.ast.expressions.{NameExpression, QualifiedNameExpression, SimpleNameExpression, MethodInvocationExpression}
import joos.ast.types.{SimpleType, ArrayType, PrimitiveType, Type}
import joos.ast.visitor.AstVisitor
import joos.semantic.types.TypeCheckingException

trait MethodInvocationExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  def getMethodFromType(t: Type, methodName: SimpleNameExpression): Type = {
    t match {
      case _: PrimitiveType => throw new TypeCheckingException("method invocation 2")
      case ArrayType(inner, dimensions) => if (methodName.standardName equals "length") PrimitiveType
          .IntegerType
      else throw new TypeCheckingException("method invocation 1")
      case s: SimpleType => {
        // TODO: Match method name to correctly overloaded one
        s.declaration.get.containedMethods.get(methodName) match {
          case Some(methods) => methods.head.returnType.get
          // TODO: Exception
          case None => throw new TypeCheckingException("method invocation 3")
        }
      }
    }
  }

  private def getMethodFromQualifiedName(methodName: QualifiedNameExpression) {
    var names = methodName.unfold
    var typeIndex = 1
    var declarationType: Type = null

    // (1) Check local variable
    require(blockEnvironment != null)
    blockEnvironment.getVariable(names.head) match {
      case Some(localVariable) => declarationType = localVariable.declarationType
    }
  }


  // Link a method that is called as an isolated expression
  private def linkMethod(methodName: NameExpression) {
    methodName match {
      // Must be a local method declaration
      case methodName: SimpleNameExpression => getMethodFromType(unit.typeDeclaration.get.asType, methodName)
      case methodName: QualifiedNameExpression => getMethodFromQualifiedName(methodName)
    }
  }

  private def linkMethod(methodName: NameExpression,

  type)

  override def apply(methodInvocationExpression: MethodInvocationExpression) {
    methodInvocationExpression.arguments.foreach(
      expr => {
        expr.accept(this)
        require(expr.declarationType != null)
      }
    )
    methodInvocationExpression.expression.foreach(
      expr => {
        expr.accept(this)
        require(expr.declarationType != null)
      }
    )

    methodInvocationExpression.expression match {
      case None => methodInvocationExpression.declarationType = linkMethod(methodInvocationExpression.methodName, unit.typeDeclaration.get.asType)
      case Some(expression) => linkMethod(methodInvocationExpression.methodName)
    }
    var currentType: Type = unit.typeDeclaration.get.asType
    if (methodInvocationExpression.expression.isDefined) {
      currentType = methodInvocationExpression.expression.get.declarationType
    }

    methodInvocationExpression.methodName match {
      case s: SimpleNameExpression => {
        methodInvocationExpression.declarationType = getMethodFromType(currentType, s)
      }
      case q: QualifiedNameExpression =>


    }
  }


  // TODO:
}

}
