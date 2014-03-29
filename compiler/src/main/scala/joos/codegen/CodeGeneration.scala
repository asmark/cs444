package joos.codegen

import joos.ast.AbstractSyntaxTree
import joos.ast.declarations.ModuleDeclaration

object CodeGeneration {

  def getCodeGenerator(ast: AbstractSyntaxTree)(implicit module: ModuleDeclaration) = {
    implicit val unit = ast.root
    Seq(
      new CodeGenerationVisitor
    )
  }


  def apply(asts: Seq[AbstractSyntaxTree]) {
    implicit val module = new ModuleDeclaration

    val numCodeGenerators = 1
    for (i <- Range(0, numCodeGenerators)) {
      for (ast <- asts) {
        ast dispatch getCodeGenerator(ast).apply(i)
      }
    }
  }

}
