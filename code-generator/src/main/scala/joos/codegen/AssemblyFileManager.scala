package joos.codegen

import collection.mutable
import java.io.PrintWriter
import joos.assemgen._

class AssemblyFileManager(val writer: PrintWriter) {
  val globals = mutable.HashSet[AssemblyLine]()
  // global
  val externs = mutable.HashSet[AssemblyLine]()
  // extern
  val data = mutable.HashSet[AssemblyLine]()
  var text = Seq.empty[AssemblyLine]

  private val sectionFormatString = "--- %s ---"

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

    // TODO: What do print here?

    writer.flush()
    writer.close()
  }
}

object AssemblyFileManager {
  def apply(writer: PrintWriter) = new AssemblyFileManager(writer)
}
