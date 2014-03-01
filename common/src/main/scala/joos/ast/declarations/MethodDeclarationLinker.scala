package joos.ast.declarations

trait MethodDeclarationLinker {
  self: MethodDeclaration =>

  def link(implicit moduleDeclaration: ModuleDeclaration, typeDeclaration: TypeDeclaration): this.type = {

    this
  }
}
