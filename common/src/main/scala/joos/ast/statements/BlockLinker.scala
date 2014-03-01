package joos.ast.statements

import joos.ast.declarations.TypeDeclaration
import joos.ast.{MethodBodyLinker, Block, CompilationUnit}
import joos.semantic.BlockEnvironment

trait BlockLinker extends MethodBodyLinker {
  self: Block =>

  override def link(implicit compilationUnit: CompilationUnit, typeDeclaration: TypeDeclaration, enclosingBlock: BlockEnvironment): this.type = {
    super.link
//    this.environment = enclosingBlock.newScope()
    this.statements.foreach(_.link)
    this
  }
}
