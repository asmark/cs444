package joos.codegen

import joos.ast.AbstractSyntaxTree
import joos.ast.declarations.ModuleDeclaration
import java.io.{File, PrintWriter}

object CodeGeneration {

  final val OutputDirectory = "output"

  def getCodeStream(ast: AbstractSyntaxTree) = new PrintWriter(new File(s"${OutputDirectory}/${ast.name}.s"))

  def getCodeGenerator(ast: AbstractSyntaxTree)(implicit module: ModuleDeclaration) = {
    implicit val unit = ast.root
    implicit val codeStream = getCodeStream(ast)
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
