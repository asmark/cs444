package joos.ast.visitor

import joos.ast.CompilationUnit

trait CompilationUnitUserBuilder[T <: (() => Unit)] {
  def build(unit: CompilationUnit): T
}
