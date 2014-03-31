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
    val writer = new PrintWriter(new File(s"${OutputDirectory}/${assemblyManager.fileName}"))

    writer.print(#: (sectionFormatString.format("Text")))
    writer.print(section(AssemblySection.Text))

    writer.print(#: ("Defining Exported Symbols"))
    assemblyManager.globals foreach (symbol => writer.print(global(symbol)))
    writer.print(emptyLine)

    writer.print(#: ("Defining Imported Symbols"))
    // Do not extern things that are globalled here
    namespace.externs -- assemblyManager.globals foreach (symbol => writer.print(extern(symbol)))
    writer.print(emptyLine)

    writer.print(#: ("Defining body"))
    writer.print(emptyLine)

    assemblyManager.text foreach writer.print
    writer.print(emptyLine)

    writer.print(#: (sectionFormatString.format("Data")))
    writer.print(section(AssemblySection.Data))
    assemblyManager.data foreach writer.print
    writer.print(emptyLine)

    writer.flush()
    writer.close()
  }
}

object AssemblyCodeGeneratorEnvironment {
  private val sectionFormatString = "--- %s ---"
  private val OutputDirectory = "output"
}
