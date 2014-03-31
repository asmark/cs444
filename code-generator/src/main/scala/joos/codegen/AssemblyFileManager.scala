package joos.codegen

import collection.mutable
import java.io.PrintWriter
import joos.assemgen._
import scala.collection.immutable.Queue

class AssemblyFileManager(val writer: PrintWriter) {
  private var globals = Set.empty[AssemblyLine]
  private var externs = Set(
    extern(new LabelReference("__malloc")),
    extern(new LabelReference("__exception")),
    extern(new LabelReference("NATIVEjava.io.OutputStream.nativeWrite"))
  )

  private val data = mutable.MutableList.empty[AssemblyLine]
  private val text = mutable.MutableList.empty[AssemblyLine]

  private val sectionFormatString = "--- %s ---"

  def appendText(assemblyLine: AssemblyLine) {
    text += assemblyLine
  }

  def appendGlobal(assemblyLine: AssemblyLine) {
    globals += assemblyLine
  }

  def appendExtern(assemblyLine: AssemblyLine) {
    externs += assemblyLine
  }

  def appendData(assemblyLine: AssemblyLine) {
    data += assemblyLine
  }

  def print = {

    writer.print(comment(sectionFormatString.format("Text")))
    writer.print(section(AssemblySection.Text))

    writer.print(comment("Defining Exported Symbols"))
    globals.foreach(global => writer.print(global.toString))
    writer.print(emptyLine().toString)

    writer.print(comment("Defining Imported Symbols"))
    externs.foreach(extern => writer.print(extern.toString))
    writer.print(emptyLine().toString)

    writer.print(comment("Defining type environment"))

    text.foreach(writer.print)
    writer.print(emptyLine())

    writer.print(comment(sectionFormatString.format("Data")))
    writer.print(section(AssemblySection.Data).toString)
    writer.print(emptyLine().toString)

    // TODO: What to print here?

    writer.flush()
    writer.close()
  }
}

object AssemblyFileManager {
  def apply(writer: PrintWriter) = new AssemblyFileManager(writer)
}
