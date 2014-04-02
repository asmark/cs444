package joos.codegen

import AssemblyCodeGeneratorEnvironment._
import java.io.{File, PrintWriter}
import joos.assemgen._
import joos.ast.AbstractSyntaxTree
import joos.ast.expressions.SimpleNameExpression
import joos.codegen.generators.TypeDeclarationCodeGenerator
import scala.collection.mutable
import joos.ast.declarations.{TypeDeclaration, MethodDeclaration}

/**
 * Stores the environment passed to code generator
 */
class AssemblyCodeGeneratorEnvironment(
    val assemblyManager: AssemblyFileManager,
    val namespace: AssemblyNamespace,
    val sitManager: SITManager) {

  def this(ast: AbstractSyntaxTree, namespace: AssemblyNamespace, sitManager: SITManager) = {
    this(new AssemblyFileManager(s"${ast.name}.s"), namespace, sitManager)

    implicit val environment = this
    ast.root.typeDeclaration match {
      case None =>
      case Some(typeDeclaration) => new TypeDeclarationCodeGenerator(typeDeclaration).generate
    }
  }

  var methodEnvironment: MethodDeclaration = null
  var typeEnvironment: TypeDeclaration = null

  def write {
    val writer = new AssemblyCodeWriter(new PrintWriter(new File(s"${OutputDirectory}/${assemblyManager.fileName}")))
    writer.write(:#(sectionFormatString.format("Text")))
    writer.write(section(AssemblySection.Text))

    writer.write(:#("Defining Exported Symbols"))
    assemblyManager.globals foreach (symbol => writer.write(global(symbol)))
    writer.write(emptyLine)

    writer.write(:#("Defining Imported Symbols"))
    // Do not extern things that are globalled here
    namespace.externs -- assemblyManager.globals foreach (symbol => writer.write(extern(symbol)))
    writer.write(emptyLine)

    writer.write(:#("Defining body"))
    writer.write(emptyLine)

    writer.write(assemblyManager.text: _*)
    writer.write(emptyLine)

    writer.write(:#(sectionFormatString.format("Data")))
    writer.write(section(AssemblySection.Data))
    writer.write(assemblyManager.data: _*)
    writer.write(emptyLine)

    if (!sitTableWritten) {
      val sortedTypeSeq = sitManager.typeIds.toSeq.sortWith((left, right) => left._2 < right._2).map(pair => pair._1)
      val sortedMapSeq = sitManager.methodIds.toSeq.sortWith((left, right) => left._2 < right._2).map(pair => pair._1)

      writer.write(#:(sectionFormatString.format("SIT")))
      writer.write("SIT"::)

      // TODO: should make sure they are really sorted in ascending order
      sortedTypeSeq.foreach(
        tipePair => {
          sortedMapSeq.foreach(
            methodPair => {
              if (tipePair.methodMap.values.toSet.contains(methodPair)) {
                writer.write("dd " + methodPair.uniqueName)
              } else {
                writer.write("dd " + 0)
              }
            }
          )
        }
      )

      sitTableWritten = true
    }

    writer.flush()
    writer.close()
  }
}

object AssemblyCodeGeneratorEnvironment {
  private val sectionFormatString = "--- %s ---"
  private val OutputDirectory = "output"
  private var sitTableWritten = false // TODO: too lazy to do it in the right way
}
