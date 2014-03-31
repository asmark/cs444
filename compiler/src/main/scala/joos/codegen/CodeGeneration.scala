package joos.codegen

import java.io.{File, PrintWriter}
import joos.ast.AbstractSyntaxTree
import joos.ast.declarations.ModuleDeclaration
import joos.codegen.generators.AssemblyCodeGenerator

object CodeGeneration {

  final val OutputDirectory = "output"

  def getAssemblyManager(ast: AbstractSyntaxTree) = AssemblyFileManager(new PrintWriter(new File(s"${OutputDirectory}/${ast.name}.s")))

  def apply(asts: Seq[AbstractSyntaxTree]) {
    implicit val module = new ModuleDeclaration

    // TODO: Link in a global namespace to AssemblyCodeGeneratorEnvironment

    asts.foreach {
      ast =>
        implicit val environment = new AssemblyCodeGeneratorEnvironment(getAssemblyManager(ast))
        AssemblyCodeGenerator(ast)
    }
  }

}
