package joos.ast.visitor

import joos.ast.CompilationUnit

trait AbstractSyntaxTreeVisitorBuilder[T <: AstVisitor] {
  def build(implicit unit: CompilationUnit): T
}
