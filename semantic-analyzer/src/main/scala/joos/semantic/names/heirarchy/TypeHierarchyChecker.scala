package joos.semantic.names.heirarchy

import joos.ast.declarations.TypeDeclaration
import joos.ast.expressions.NameExpression
import joos.semantic.SemanticException
import joos.core.Logger
import scala.language.implicitConversions
import joos.ast._
import scala.Some

trait TypeHierarchyChecker {

   protected implicit def resolveType(typeName: NameExpression)(implicit compilationUnit: CompilationUnit) = {
     compilationUnit.getVisibleType(typeName) match {
       case None => {
         Logger.logError("All types SHOULD be resolved by now")
         throw new SemanticException(s"Somehow ${typeName} was not able to be resolved")
       }
       case Some(typed) => typed
     }
   }

  protected implicit def typeDeclarationToUnit(typeDeclaration: TypeDeclaration) = {
    require(typeDeclaration != null)
    typeDeclaration.compilationUnit
  }

  protected def typeEquality(type1: TypeDeclaration, type2: TypeDeclaration) = {
    if (type1.packageDeclaration == null || type2.packageDeclaration == null) {
      Logger.logError(s"${type1.name.standardName} or ${type2.name.standardName} had null package declarations")
    }
    (type1.packageDeclaration, type1) equals(type2.packageDeclaration, type2)
  }

  protected def typeEquality(type1: Type, type2: Type)(implicit compilationUnit: CompilationUnit): Boolean = {
    require(compilationUnit != null)
    (type1, type2) match {
      case (SimpleType(name1), SimpleType(name2)) => typeEquality(name1, name2)
      case (ArrayType(name1, dimensions1), ArrayType(name2, dimensions2)) =>
        dimensions1 == dimensions2 && typeEquality(name1, name2)
      case (PrimitiveType(type1), PrimitiveType(type2)) => type1 equals type2
    }
  }

 }
