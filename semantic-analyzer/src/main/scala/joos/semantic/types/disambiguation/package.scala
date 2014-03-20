package joos.semantic.types

import joos.ast.declarations._
import joos.ast.expressions._
import joos.ast.types.{SimpleType, ArrayType, PrimitiveType, Type}
import joos.ast.{CompilationUnit, Modifier}
import joos.core.Enumeration
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
      if (method.parameters(i).variableType != parameters(i).expressionType) return false
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
      case Visibility.Local => if (declaration.modifiers contains Modifier.Static)
        throw new InvalidStaticUseException(declaration.declarationName)
      case Visibility.Static => if (!(declaration.modifiers contains Modifier.Static))
        throw new InvalidStaticUseException(declaration.declarationName)
    }
  }

  def checkSimpleAccess(field: FieldDeclaration)(implicit unit: CompilationUnit) {
    if (field.modifiers contains Modifier.Protected) {
      val selfType = unit.typeDeclaration.get
      if (!(selfType.packageDeclaration.declarationName equals field.typeDeclaration.packageDeclaration.declarationName)) {
        if (!(selfType.allAncestors contains field.typeDeclaration)) {
          throw new IllegalProtectedAccessException(field.declarationName)
        }
      }
    }
  }

  def checkQualifiedAccess(field: FieldDeclaration, prefixType: Type)(implicit unit: CompilationUnit) {
    checkSimpleAccess(field)
    if ((field.modifiers contains Modifier.Protected) && (!(field.modifiers contains Modifier.Static))) {
      val selfType = unit.typeDeclaration.get
      if (!(selfType.packageDeclaration.declarationName equals field.typeDeclaration.packageDeclaration.declarationName)) {
        if (!(prefixType.declaration equals selfType)) {
          if (!(prefixType.declaration.allAncestors contains selfType)) {
            throw new IllegalProtectedAccessException(field.declarationName)
          }
        }
      }
    }
  }

  def checkSimpleAccess(method: MethodDeclaration)(implicit unit: CompilationUnit) {
    if (method.modifiers contains Modifier.Protected) {
      val selfType = unit.typeDeclaration.get
      if (!(selfType.packageDeclaration.declarationName equals method.typeDeclaration.packageDeclaration.declarationName)) {
        if (!(selfType.allAncestors contains method.typeDeclaration)) {
          throw new IllegalProtectedAccessException(method.declarationName)
        }
      }
    }
  }

  def checkQualifiedAccess(method: MethodDeclaration, prefixType: Type)(implicit unit: CompilationUnit) {
    checkSimpleAccess(method)
    if ((method.modifiers contains Modifier.Protected) && (!(method.modifiers contains Modifier.Static))) {
      val selfType = unit.typeDeclaration.get
      if (!(selfType.packageDeclaration.declarationName equals method.typeDeclaration.packageDeclaration.declarationName)) {
        if (!(prefixType.declaration equals selfType)) {
          if (!(prefixType.declaration.allAncestors contains selfType)) {
            throw new IllegalProtectedAccessException(method.declarationName)
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
        val caller = s.declaration
        findMethod(methodName, parameters, caller.containedMethods) match {
          case None => None
          case Some(methodDeclaration) => {
            checkVisibility(methodDeclaration, visibility)
            methodDeclaration.returnType
          }
        }
      }
    }
  }

  def getMethodFromType(t: Type, methodName: SimpleNameExpression, parameters: IndexedSeq[Expression])
      (implicit unit: CompilationUnit): Option[MethodDeclaration] = {
    t match {
      case _: PrimitiveType | ArrayType(_, _) => None
      case s: SimpleType => {
        val caller = s.declaration
        findMethod(methodName, parameters, caller.containedMethods) match {
          case None => None
          case Some(methodDeclaration) => Some(methodDeclaration)
        }
      }
    }
  }

  def getFieldTypeFromType(t: Type, fieldName: SimpleNameExpression, visibility: Visibility)(implicit unit: CompilationUnit): Option[Type] = {
    t match {
      case _: PrimitiveType => None
      case ArrayType(_, _) => if (fieldName.standardName equals "length") {
        Some(PrimitiveType.IntegerType)
      } else None
      case s: SimpleType => {
        val caller = s.declaration
        caller.containedFields.get(fieldName) match {
          case None => None
          case Some(fieldDeclaration) => {
            checkVisibility(fieldDeclaration, visibility)
            Some(fieldDeclaration.variableType)
          }
        }
      }
    }
  }

  def getFieldFromType(t: Type, fieldName: SimpleNameExpression)(implicit unit: CompilationUnit): Option[FieldDeclaration] = {
    t match {
      case _: PrimitiveType => None
      // IMPORTANT: has no type declaration attached
      case ArrayType(_, _) => Some(
        FieldDeclaration(
          Seq(Modifier.Final),
          PrimitiveType.IntegerType,
          VariableDeclarationFragment(SimpleNameExpression("length"), None)))
      case s: SimpleType => {
        val caller = s.declaration
        caller.containedFields.get(fieldName) match {
          case None => None
          case Some(fieldDeclaration) => Some(fieldDeclaration)
        }
      }
    }
  }

}
