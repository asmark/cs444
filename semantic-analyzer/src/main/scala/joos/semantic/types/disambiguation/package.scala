package joos.semantic.types

import joos.ast.declarations.{FieldDeclaration, TypeDeclaration, BodyDeclaration, MethodDeclaration}
import joos.ast.expressions._
import joos.ast.types.{SimpleType, ArrayType, PrimitiveType, Type}
import joos.ast.{CompilationUnit, Modifier}
import joos.core.Enumeration
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

  class Visibility(val name: String) extends Visibility.Value

  object Visibility extends Enumeration {
    type T = Visibility

    final val Local = this + new Visibility("Local")
    final val Static = this + new Visibility("Static")
  }

  def checkVisibility[T <: BodyDeclaration](declaration: T, visibility: Visibility) {
    visibility match {
      case Visibility.Local => if (declaration.modifiers contains Modifier.Static) visibility
//              throw new InvalidStaticUseException(declaration.declarationName)
      case Visibility.Static => if (!(declaration.modifiers contains Modifier.Static))
              throw new InvalidStaticUseException(declaration.declarationName)
    }
  }

  def checkAccess[T <: BodyDeclaration](declaration: T, owner: TypeDeclaration)(implicit unit: CompilationUnit) {
    if (declaration.modifiers contains Modifier.Protected) {
      val selfType = unit.typeDeclaration.get
      if (!(selfType.packageDeclaration.declarationName equals owner.packageDeclaration.declarationName)) {
        if (!(selfType.allAncestors contains owner)) {
          if (owner.allAncestors contains selfType) {
//            declaration match {
//              case m: MethodDeclaration => if (!selfType.containedMethods.contains(m.name)) throw new IllegalProtectedAccessException(
//                declaration
//                    .declarationName)
//              case f: FieldDeclaration => if (!selfType.containedFields.contains(f.declarationName)) throw new IllegalProtectedAccessException(
//                declaration.declarationName)
//            }
          }
        }
      }
    }
  }

  def getMethodTypeFromType(t: Type, methodName: SimpleNameExpression, parameters: IndexedSeq[Expression], visibility: Visibility)
      (implicit unit: CompilationUnit): Option[Type] = {
    t match {
      case _: PrimitiveType | ArrayType(_, _) => None
      case s: SimpleType => {
        val owner = s.declaration.get
        findMethod(methodName, parameters, owner.containedMethods) match {
          case None => None
          case Some(methodDeclaration) => {
            checkAccess(methodDeclaration, owner)
            checkVisibility(methodDeclaration, visibility)
            methodDeclaration.returnType
          }
        }
      }
    }
  }

  //  def getMethodFromType(t: Type, methodName: SimpleNameExpression, parameters: IndexedSeq[Expression])(implicit unit: CompilationUnit):
  // Option[MethodDeclaration] = {
  //    t match {
  //      case _: PrimitiveType | ArrayType(_, _) => None
  //      case s: SimpleType => {
  //        val containedMethods = s.declaration.get.containedMethods
  //        findMethod(methodName, parameters, containedMethods) match {
  //          case None => None
  //          case Some(method) => Some(method)
  //        }
  //      }
  //    }
  //  }

  def getFieldTypeFromType(t: Type, fieldName: SimpleNameExpression, visibility: Visibility)(implicit unit: CompilationUnit): Option[Type] = {
    t match {
      case _: PrimitiveType => None
      case ArrayType(_, _) => if (fieldName.standardName equals "length") {
        Some(PrimitiveType.IntegerType)
      } else None
      case s: SimpleType => {
        val owner = s.declaration.get
        owner.containedFields.get(fieldName) match {
          case None => None
          case Some(fieldDeclaration) => {
            checkAccess(fieldDeclaration, owner)
            checkVisibility(fieldDeclaration, visibility)
            Some(fieldDeclaration.variableType)
          }
        }
      }

    }

  }

}
