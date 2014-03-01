package joos.ast.statements

import joos.ast.{MethodBodyLinker, IfStatement, CompilationUnit}
import joos.ast.declarations.TypeDeclaration
import joos.semantic.BlockEnvironment

trait IfStatementLinker extends MethodBodyLinker {
  self: IfStatement =>

  override def link(implicit compilationUnit: CompilationUnit, typeDeclaration: TypeDeclaration, enclosingBlock: BlockEnvironment): this.type = {
    super.link
    this.condition.link
    this.trueStatement.link
    this.falseStatement.map(_.link)
    this
  }
}
