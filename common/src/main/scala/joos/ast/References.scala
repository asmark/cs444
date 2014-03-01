package joos.ast

import joos.core.ReferenceManager
import joos.ast.declarations.{MethodDeclaration, TypeDeclaration}

object TypeDeclarationReference extends ReferenceManager[TypeDeclaration]
object MethodDeclarationReference extends ReferenceManager[MethodDeclaration]
object CompilationUnitReference extends ReferenceManager[CompilationUnit]
