package joos

import joos.ast._
import joos.ast.declarations.TypeDeclaration
import joos.ast.expressions.NameExpression
import joos.core.Logger
import scala.Some

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

  def areEqual(type1: TypeDeclaration, type2: TypeDeclaration): Boolean = {
    if (type1.packageDeclaration == null || type2.packageDeclaration == null) {
      Logger.logError(s"${type1.name} or ${type2.name} had null package declarations")
    }

    (type1.packageDeclaration eq type2.packageDeclaration) && (type1 eq type2)
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
}
