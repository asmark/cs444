package joos.semantic.types.checking

import joos.ast.declarations.MethodDeclaration
import joos.ast.expressions._
import joos.ast.types.Type
import joos.ast.visitor.AstVisitor
import joos.ast.Modifier
import joos.semantic.types.disambiguation.Visibility._
import joos.semantic.types.disambiguation._

trait MethodInvocationExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  private def getQualifiedMethod(methodAccess: QualifiedNameExpression, parameters: IndexedSeq[Expression]) {

    val unfolded = methodAccess.unfold

    val (fieldPrefix, methodName) = (fold(unfolded.dropRight(1)), unfolded.last)

    val visibility = resolveFieldAccess(fieldPrefix)

    getMethodFromType(fieldPrefix.expressionType, methodName, parameters) match {
      case Some(methodDeclaration) => {
        checkVisibility(methodDeclaration, visibility)
        checkQualifiedAccess(methodDeclaration, fieldPrefix.expressionType)
        methodAccess.declaration = methodDeclaration
        methodAccess.expressionType = methodDeclaration.returnType.get
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
            methodName.declaration = method
            methodName.expressionType = method.returnType.get
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
            methodName.declaration = method
            methodName.expressionType = method.returnType.get
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
                methodName.declaration = method
                methodName.expressionType = method.returnType.get
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
        require(expr.expressionType != null)
      }
    )
    invocation.expression.foreach(
      expr => {
        expr.accept(this)
        require(expr.expressionType != null)
      }
    )

    invocation.expression match {
      case None => linkMethod(invocation.methodName, invocation.arguments)
      case Some(expression) => linkMethod(expression.expressionType, invocation.methodName, invocation.arguments)
    }

    invocation.methodName match {
      case name: SimpleNameExpression =>
      case name: QualifiedNameExpression =>
        val linker = new FieldNameLinker(invocation.expression, name.qualifier)
        linker()
    }

    invocation.expressionType = invocation.methodName.expressionType
    invocation.declaration = invocation.methodName.declaration.asInstanceOf[MethodDeclaration]
  }
}
