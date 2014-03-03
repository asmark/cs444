package joos.semantic

import joos.ast.declarations.{PackageDeclaration, TypeDeclaration}
import joos.core.Logger
import scala.collection.mutable
import joos.ast.Modifier

object EnvironmentComparisons {

  def typeEquality(type1: TypeDeclaration, type2: TypeDeclaration) = {
    if (type1.packageDeclaration == null || type2.packageDeclaration == null) {
      Logger.logError(s"${type1.name.standardName} or ${type2.name.standardName} had null package declarations")
    }
    (type1.packageDeclaration, type1) equals(type2.packageDeclaration, type2)
  }

  def allDifferent(types: Seq[TypeDeclaration]): Boolean = {
    val typeSet = mutable.HashSet.empty[(PackageDeclaration, TypeDeclaration)]
    types foreach {
      typeDeclaration =>
        if (!typeSet.add((typeDeclaration.packageDeclaration, typeDeclaration))) return false
    }
    return true
  }

  def containsModifier(modifiers: Seq[Modifier], contained: Modifier) = modifiers contains contained

}
