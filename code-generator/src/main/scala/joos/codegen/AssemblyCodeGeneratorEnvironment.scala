package joos.codegen

import AssemblyCodeGeneratorEnvironment._
import java.io.{File, PrintWriter}
import joos.assemgen._
import joos.ast.AbstractSyntaxTree
import joos.codegen.generators.TypeDeclarationCodeGenerator

/**
 * Stores the environment passed to code generator
 */
class AssemblyCodeGeneratorEnvironment(val assemblyManager: AssemblyFileManager, val namespace: AssemblyNamespace) {
  def this(ast: AbstractSyntaxTree, namespace: AssemblyNamespace) = {
    this(new AssemblyFileManager(s"${ast.name}.s"), namespace)

    implicit val environment = this
    ast.root.typeDeclaration match {
      case None =>
      case Some(typeDeclaration) => new TypeDeclarationCodeGenerator(typeDeclaration).generate
    }
  }

  def write {
    val writer = new AssemblyCodeWriter(new PrintWriter(new File(s"${OutputDirectory}/${assemblyManager.fileName}")))
    writer.write(#:(sectionFormatString.format("Text")))
    writer.write(section(AssemblySection.Text))

    writer.write(#:("Defining Exported Symbols"))
    assemblyManager.globals foreach (symbol => writer.write(global(symbol)))
    writer.write(emptyLine)

    writer.write(#:("Defining Imported Symbols"))
    // Do not extern things that are globalled here
    namespace.externs -- assemblyManager.globals foreach (symbol => writer.write(extern(symbol)))
    writer.write(emptyLine)

    writer.write(#:("Defining body"))
    writer.write(emptyLine)

    writer.write(assemblyManager.text: _*)
    writer.write(emptyLine)

    writer.write(#:(sectionFormatString.format("Data")))
    writer.write(section(AssemblySection.Data))
    writer.write(assemblyManager.data: _*)
    writer.write(emptyLine)

    writer.flush()
    writer.close()
  }
}

object AssemblyCodeGeneratorEnvironment {
  private val sectionFormatString = "--- %s ---"
  private val OutputDirectory = "output"
}
