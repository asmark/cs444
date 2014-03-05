package joos.semantic.names.heirarchy

import joos.ast._
import joos.ast.declarations.TypeDeclaration
import joos.ast.expressions.NameExpression
import joos.core.Logger
import joos.semantic.SemanticException
import scala.Some
import scala.language.implicitConversions

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

 }
