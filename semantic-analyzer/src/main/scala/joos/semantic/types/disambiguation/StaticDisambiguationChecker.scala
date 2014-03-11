package joos.semantic.types.disambiguation

import joos.ast.declarations.TypeDeclaration
import joos.ast.visitor.AstCompleteVisitor
import joos.ast.CompilationUnit

class StaticDisambiguationChecker(implicit unit: CompilationUnit) extends AstCompleteVisitor {

  override def apply(declaration: TypeDeclaration) {
    declaration.fields foreach {
      field =>
        new ForwardFieldChecker(declaration.containedFields).apply(field)
        declaration.add(field)
    }
  }
}
