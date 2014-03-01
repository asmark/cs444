package joos.ast.statements

import joos.ast.declarations.TypeDeclaration
import joos.ast.{MethodBodyLinker, WhileStatement, CompilationUnit}
import joos.semantic.BlockEnvironment

trait WhileStatementLinker extends MethodBodyLinker {
  self: WhileStatement =>

  override def link(implicit compilationUnit: CompilationUnit, typeDeclaration: TypeDeclaration, enclosingBlock: BlockEnvironment): this.type = {
    super.link
    this.condition.link
    this.body.link
    this
  }
}
