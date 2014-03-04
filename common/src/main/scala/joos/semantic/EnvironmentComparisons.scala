package joos.semantic

import joos.ast.declarations.{PackageDeclaration, TypeDeclaration}
import joos.core.Logger
import scala.collection.mutable
import joos.ast.{SimpleType, CompilationUnit, Type, Modifier}

object EnvironmentComparisons {

  def typeEquality(type1: TypeDeclaration, type2: TypeDeclaration) = {
    if (type1.packageDeclaration == null || type2.packageDeclaration == null) {
      Logger.logError(s"${type1.name.standardName} or ${type2.name.standardName} had null package declarations")
    }
    (type1.packageDeclaration, type1) equals(type2.packageDeclaration, type2)
  }

  def getQualifiedName(typeDeclaration:TypeDeclaration) = {
    require(typeDeclaration.packageDeclaration != null)
    typeDeclaration.packageDeclaration.name.standardName + "." + typeDeclaration.name.standardName
  }

  def isJavaLangObject(typeDeclaration: TypeDeclaration) = {
    getQualifiedName(typeDeclaration) equals "java.lang.Object"
  }

  def findDuplicate[T](sequence: Iterable[T]): Option[T] = {
    val set = mutable.HashSet.empty[T]
    sequence foreach {
      element =>
        if (!set.add(element)) {
          return Some(element)
        }
    }
    return None
  }

}
