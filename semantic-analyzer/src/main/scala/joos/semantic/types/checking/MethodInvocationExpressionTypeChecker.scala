package joos.semantic.types.checking

import joos.ast.expressions._
import joos.ast.visitor.AstVisitor
import joos.semantic.types.disambiguation._

trait MethodInvocationExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  private def getStaticAccessMethod(methodAccess: QualifiedNameExpression, parameters: IndexedSeq[Expression]) {

    val unfolded = methodAccess.unfold

    val (fieldPrefix, methodName) = (fold(unfolded.dropRight(1)), unfolded.last)

    resolveStaticFieldAccess(fieldPrefix.asInstanceOf[QualifiedNameExpression])

    getMethodFromType(fieldPrefix.declarationType, methodName, parameters) match {
      case Some(returnType) => methodAccess.declarationType = returnType
      case None => throw new AmbiguousNameException(methodAccess)
    }
    //
    //    var typeIndex = 1
    //    var declarationType: Type = null
    //
    //    // (1) Check local variable
    //    require(blockEnvironment != null)
    //    blockEnvironment.getVariable(names.head) match {
    //      case Some(localVariable) => declarationType = localVariable.declarationType
    //      case None =>
    //
    //        // (2) Check local field
    //        typeEnvironment.containedFields.get(names.head) match {
    //          case Some(field) => {
    //            declarationType = field.declarationType
    //            if (field.isStatic) {
    //              throw new InvalidStaticUseException(methodName)
    //            }
    //          }
    //          case None => {
    //
    //            // (3) Check static accesses
    //
    //            // Must have a prefix that is a valid type
    //            while (unit.getVisibleType(names.take(typeIndex)).isEmpty) {
    //              typeIndex += 1
    //              if (typeIndex > names.length) {
    //                throw new AmbiguousNameException(methodName)
    //              }
    //            }
    //
    //            val typeName = unit.getVisibleType(names.take(typeIndex)).get
    //            declarationType = typeName.asType
    //            // Next name must be a static field
    //
    //            if (names.size > typeIndex) {
    //              val fieldName = names(typeIndex)
    //              typeName.containedFields.get(fieldName) match {
    //                case Some(field) =>
    //                  if (!field.isStatic) {
    //                    throw new InvalidStaticUseException(methodName)
    //                  }
    //                  declarationType = field.variableType
    //                  typeIndex += 1
    //              }
    //            }
    //          }
    //        }
    //    }
    //
    //    // All remaining names must be instance field acceses
    //    names = names.drop(typeIndex)
    //    names foreach {
    //      name =>
    //        declarationType match {
    //          case _: ArrayType | PrimitiveType =>
    //            throw new AmbiguousNameException(methodName)
    //          case simpleType: SimpleType => {
    //            simpleType.declaration.get.containedFields.get(name) match {
    //              case None => throw new AmbiguousNameException(name)
    //              case Some(field) => {
    //                if (field.isStatic) {
    //                  throw new InvalidStaticUseException(name)
    //                }
    //                declarationType = field.variableType
    //              }
    //            }
    //          }
    //        }
  }

  private def linkMethod(left: Expression, methodName: NameExpression, parameters: IndexedSeq[Expression]) {
    require(left.declarationType != null)
    methodName match {
      case methodName: SimpleNameExpression => {
        getMethodFromType(left.declarationType, methodName, parameters) match {
          case None => throw new AmbiguousNameException(methodName)
          case Some(returnType) => methodName.declarationType = returnType
        }
      }

      case methodName: QualifiedNameExpression => {
        var leftType = left.declarationType
        methodName.unfold.dropRight(1) foreach {
          name =>
            getFieldFromType(leftType, name) match {
              case None => throw new AmbiguousNameException(methodName)
              case Some(returnType) => {
                leftType = returnType
              }
            }
            getMethodFromType(leftType, methodName.unfold.last, parameters) match {
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
        getMethodFromType(unit.typeDeclaration.get.asType, methodName, parameters) match {
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
      case Some(expression) => linkMethod(expression, invocation.methodName, invocation.arguments)
    }
  }


}
