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

    writer.print(comment(sectionFormatString.format("Text")))
    writer.print(section(AssemblySection.Text))

    writer.print(comment("Defining Exported Symbols"))
    assemblyManager.globals.foreach(global => writer.print(global))
    writer.print(emptyLine)

    writer.print(comment("Defining Imported Symbols"))
    namespace.externs.foreach(extern => writer.print(extern))
    writer.print(emptyLine)

    writer.print(comment("Defining body"))
    writer.print(emptyLine)

    assemblyManager.text.foreach(writer.print)
    writer.print(emptyLine)

    writer.print(comment(sectionFormatString.format("Data")))
    writer.print(section(AssemblySection.Data))
    assemblyManager.data.foreach(data => writer.print(data))
    writer.print(emptyLine)

    writer.flush()
    writer.close()
  }
}

object AssemblyCodeGeneratorEnvironment {
  private val sectionFormatString = "--- %s ---"
  private val OutputDirectory = "output"
}
