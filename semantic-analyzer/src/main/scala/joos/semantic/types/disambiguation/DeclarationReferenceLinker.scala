package joos.semantic.types.disambiguation

import joos.ast.CompilationUnit
import joos.ast.visitor.{AbstractSyntaxTreeVisitorBuilder, AstCompleteVisitor}

/**
 * Links references to its declarations
 */
class DeclarationReferenceLinker(implicit unit: CompilationUnit) extends AstCompleteVisitor {

}

object DeclarationReferenceLinker extends AbstractSyntaxTreeVisitorBuilder[DeclarationReferenceLinker] {
  override def build(implicit unit: CompilationUnit) = new DeclarationReferenceLinker
}
