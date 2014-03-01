package joos.ast.statements

import joos.ast.declarations.TypeDeclaration
import joos.ast.{ReturnStatement, CompilationUnit, MethodBodyLinker}
import joos.semantic.BlockEnvironment

trait ReturnStatementLinker extends MethodBodyLinker {
  self: ReturnStatement =>

  override def link(implicit compilationUnit: CompilationUnit, typeDeclaration: TypeDeclaration, enclosingBlock: BlockEnvironment): this.type = {
    super.link
    this.expression.map(_.link)
    this
  }
}
