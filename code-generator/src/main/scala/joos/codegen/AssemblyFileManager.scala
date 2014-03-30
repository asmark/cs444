package joos.codegen

import joos.ast.CompilationUnit
import java.io.PrintWriter
import joos.assemgen._
import collection.mutable

class AssemblyFileManager(val writer: PrintWriter) {
  val globals = mutable.HashSet[AssemblyLine]() // global
  val externs = mutable.HashSet[AssemblyLine]() // extern
  val data = mutable.HashSet[AssemblyLine]()
  val functions = mutable.HashSet[List[AssemblyLine]]()
  val classes = mutable.HashSet[List[AssemblyLine]]()

  def print = {
    writer.print(section(AssemblySection.Data).toString)
    writer.print(emptyLine().toString)

    writer.print(section(AssemblySection.Text).toString)
    writer.print(emptyLine().toString)

    globals.foreach(global => writer.print(global.toString))
    writer.print(emptyLine().toString)

    externs.foreach(extern => writer.print(extern.toString))
    writer.print(emptyLine().toString)

    functions.foreach(lines => {
      lines.foreach(
        line => writer.print(line.toString)
      )
      writer.print(emptyLine().toString)
    })

    classes.foreach(classDef => {
      classDef.foreach(
        line => writer.print(line.toString)
      )
      writer.print(emptyLine().toString)
    })
  }
}

object AssemblyFileManager {
  def apply(writer: PrintWriter) = new AssemblyFileManager(writer)
}
