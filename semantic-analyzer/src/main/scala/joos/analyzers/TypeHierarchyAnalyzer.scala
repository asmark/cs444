package joos.analyzers

import joos.ast.expressions.NameExpression
import joos.ast.declarations.TypeDeclaration

trait TypeHierarchyAnalyzer {

  implicit def getTypeFromName(typeName: NameExpression)(implicit typeDeclaration: TypeDeclaration) = {
    typeDeclaration.compilationUnit.getVisibleType(typeName)
  }

}
