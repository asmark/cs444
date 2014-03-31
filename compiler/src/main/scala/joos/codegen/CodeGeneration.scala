package joos.codegen

import java.io.{File, PrintWriter}
import joos.ast.AbstractSyntaxTree
import joos.ast.declarations.ModuleDeclaration
import joos.codegen.generators.CodeGenerationVisitor

object CodeGeneration {

  final val OutputDirectory = "output"

  def getAssemblyManager(ast: AbstractSyntaxTree) = AssemblyFileManager(new PrintWriter(new File(s"${OutputDirectory}/${ast.name}.s")))

  def getCodeGenerator(ast: AbstractSyntaxTree)(implicit module: ModuleDeclaration) = {
    implicit val unit = ast.root
    implicit val codeStream = getAssemblyManager(ast)
    new CodeGenerationVisitor
  }


  def apply(asts: Seq[AbstractSyntaxTree]) {
    implicit val module = new ModuleDeclaration

    for (ast <- asts) {
      val codeGenerator = getCodeGenerator(ast)
      ast dispatch codeGenerator
      codeGenerator.assemblyManager.print
    }
  }

}
