package joos

import joos.ast._
import joos.ast.declarations.{PackageDeclaration, TypeDeclaration}
import joos.ast.expressions.NameExpression
import joos.ast.types.{PrimitiveType, ArrayType, SimpleType, Type}
import joos.core.Logger
import scala.Some
import scala.collection.mutable

package object semantic {
  /**
   * You can only call this after the environment is built
   */
  def getTypeDeclaration(name: NameExpression)(implicit unit: CompilationUnit): TypeDeclaration = {
    unit.getVisibleType(name) match {
      case None => {
        val error = s"Cannot resolve ${name} to a type"
        Logger.logError(error)
        throw new SemanticException(error)
      }
      case Some(t) => t
    }
  }

  val javaLangObject = NameExpression("java.lang.Object")

  def getSuperType(typeDeclaration: TypeDeclaration): Option[TypeDeclaration] = {
    val compilationUnit = typeDeclaration.compilationUnit
    val theFullName = fullName(typeDeclaration)
    theFullName equals javaLangObject.standardName match {
      case true => None
      case false => {
        Some(
          typeDeclaration.superType match {
            case Some(superType) => getTypeDeclaration(superType)(compilationUnit)
            case None => compilationUnit.javaLangObjectClass
          })
      }
    }
  }

  def getSuperInterfaces(typeDeclaration: TypeDeclaration)(implicit unit: CompilationUnit): Seq[TypeDeclaration] = {
    if (typeDeclaration.isInterface && typeDeclaration.superInterfaces.isEmpty) {
      Seq(unit.javaLangObjectInterface)
    } else {
      typeDeclaration.superInterfaces.map(getTypeDeclaration)
    }
  }

  def fullName(typeDeclaration: TypeDeclaration) = {
    require(typeDeclaration.packageDeclaration != null)
    if (typeDeclaration.packageDeclaration == PackageDeclaration.DefaultPackage) {
      typeDeclaration.name.standardName
    } else {
      typeDeclaration.packageDeclaration.name.standardName + '.' + typeDeclaration.name.standardName
    }
  }

  def areEqual(type1: TypeDeclaration, type2: TypeDeclaration): Boolean = {
    if (type1.packageDeclaration == null || type2.packageDeclaration == null) {
      Logger.logError(s"${type1.name} or ${type2.name} had null package declarations")
    }

    fullName(type1) equals fullName(type2)
  }

  def areEqual(type1: Option[Type], type2: Option[Type])(implicit unit: CompilationUnit): Boolean = {
    (type1, type2) match {
      case (None, None) => true
      case (Some(type1), Some(type2)) => areEqual(type1, type2)
      case _ => false
    }
  }

  def areEqual(type1: Type, type2: Type)(implicit unit: CompilationUnit): Boolean = {
    require(unit != null)
    (type1, type2) match {
      case (SimpleType(name1), SimpleType(name2)) => areEqual(getTypeDeclaration(name1), getTypeDeclaration(name2))
      case (ArrayType(name1, dimensions1), ArrayType(name2, dimensions2)) =>
        dimensions1 == dimensions2 && areEqual(name1, name2)
      case (PrimitiveType(token1), PrimitiveType(token2)) => token1 == token2
      case _ => false
    }
  }

  def findDuplicate[T](sequence: Iterable[T]): Option[T] = {
    val set = mutable.HashSet.empty[T]
    sequence foreach {
      element =>
        if (!set.add(element)) {
          return Some(element)
        }
    }
    None
  }
}
