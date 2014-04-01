package joos.codegen

import AssemblyCodeGeneratorEnvironment._
import java.io.{File, PrintWriter}
import joos.assemgen._
import joos.ast.AbstractSyntaxTree
import joos.codegen.generators.TypeDeclarationCodeGenerator
import scala.collection.mutable
import joos.ast.expressions.SimpleNameExpression

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

  // TODO: Refactor this out later
  private val localSlots = mutable.Map.empty[SimpleNameExpression, Int]
  private var localIndex = 0
  var numLocals = 0

  def addLocalSlot(local: SimpleNameExpression) {
    localSlots.put(local, localIndex)
    localIndex += 1
  }

  private val parameterSlots = mutable.Map.empty[SimpleNameExpression, Int]
  private var parameterIndex = 0
  def addParameterSlot(parameter: SimpleNameExpression) {
    parameterSlots.put(parameter, parameterIndex)
    parameterIndex += 1
  }

  /**
   * Gets the slot used by this variable
   * TODO: Field slots
   */
  def getVariableSlot(variable: SimpleNameExpression): Int = {
    localSlots.get(variable) match {
      case Some(slot) => slot
      case None => parameterSlots.get(variable).get + numLocals
    }
  }  

  def resetVariables() {
    localSlots.clear()
    localIndex = 0
    numLocals = 0
    parameterSlots.clear()
    parameterIndex = 0
  }

}

object AssemblyCodeGeneratorEnvironment {
  private val sectionFormatString = "--- %s ---"
  private val OutputDirectory = "output"
}
