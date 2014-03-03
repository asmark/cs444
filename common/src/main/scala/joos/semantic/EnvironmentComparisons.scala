package joos.semantic

import joos.ast.declarations.TypeDeclaration
import joos.core.Logger

object EnvironmentComparisons {

  def typeEquality(type1: TypeDeclaration, type2: TypeDeclaration) = {
    if (type1.packageDeclaration == null || type2.packageDeclaration == null) {
      Logger.logError(s"${type1.name.standardName} or ${type2.name.standardName} had null package declarations")
    }
    (type1.packageDeclaration, type1) equals (type2.packageDeclaration, type2)
  }

}
