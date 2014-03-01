package joos.ast

import joos.ast.declarations.TypeDeclaration
import joos.semantic.BlockEnvironment

trait MethodBodyElement {
  var unit: CompilationUnit = null
  var typed: TypeDeclaration = null
  var block: BlockEnvironment = null
}
