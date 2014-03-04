package joos.semantic.names

import joos.ast.declarations.TypeDeclaration
import joos.ast.expressions.NameExpression

trait TypeHierarchyAnalyzer {

   protected implicit def resolveType(typeName: NameExpression)(implicit typeDeclaration: TypeDeclaration) = {
     typeDeclaration.compilationUnit.getVisibleType(typeName) match {
       case None => throw new InvalidTypeReferenceException(typeName)
       case Some(typed) => typed
     }
   }

 }
