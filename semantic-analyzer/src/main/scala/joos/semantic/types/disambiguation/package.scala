package joos.semantic.types

import joos.ast.declarations.MethodDeclaration
import joos.ast.expressions._
import joos.ast.types.{SimpleType, ArrayType, PrimitiveType, Type}
import scala.Some
import scala.language.implicitConversions

package object disambiguation {

  def findMethod(
      targetMethod: SimpleNameExpression,
      parameters: IndexedSeq[Expression],
      methods: Map[SimpleNameExpression, Set[MethodDeclaration]]): Option[MethodDeclaration] = {
    methods.get(targetMethod) match {
      case None => None
      case Some(ms) => findMethod(parameters, ms)
    }
  }

  def findMethod(parameters: IndexedSeq[Expression], methods: Iterable[MethodDeclaration]): Option[MethodDeclaration] = {
    methods.find(method => isMatch(method, parameters))
  }


  private[this] def isMatch(method: MethodDeclaration, parameters: IndexedSeq[Expression]): Boolean = {
    if (method.parameters.length != parameters.length) return false
    for (i <- 0 until parameters.length) {
      if (method.parameters(i).variableType != parameters(i).declarationType) return false
    }
    true
  }

  implicit def fold(names: Seq[SimpleNameExpression]): NameExpression = {
    names.reduceLeft {
      (tree: NameExpression, name: SimpleNameExpression) =>
        QualifiedNameExpression(tree, name)
    }
  }

  def getMethodFromType(t: Type, methodName: SimpleNameExpression, parameters: IndexedSeq[Expression]): Option[Type] = {
    t match {
      case _: PrimitiveType | ArrayType(_, _) => None
      case s: SimpleType => {
        val containedMethods = s.declaration.get.containedMethods
        findMethod(methodName, parameters, containedMethods) match {
          case None => None
          case Some(method) => method.returnType
        }
      }
    }
  }

  def getFieldFromType(t: Type, fieldName: SimpleNameExpression): Option[Type] = {
    t match {
      case _: PrimitiveType => None
      case ArrayType(_, _) => if (fieldName.standardName equals "length") {
        Some(PrimitiveType.IntegerType)
      } else None
      case s: SimpleType => {
        s.declaration.get.containedFields.get(fieldName) match {
          case None => None
          case Some(field) => Some(field.variableType)
        }
      }
    }
  }


}
