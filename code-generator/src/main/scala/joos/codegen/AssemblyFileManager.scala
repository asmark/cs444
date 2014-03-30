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
  val functions = mutable.HashSet[Seq[AssemblyLine]]()
  val classes = mutable.HashSet[Seq[AssemblyLine]]()
  var text = Seq.empty[AssemblyLine]

  private val sectionFormatString = "--- %s ---"

  def print = {
    writer.print(comment(sectionFormatString.format("Data")))
    writer.print(section(AssemblySection.Data).toString)
    writer.print(emptyLine().toString)

    writer.print(comment(sectionFormatString.format("Exported Symbols")))
    globals.foreach(global => writer.print(global.toString))
    writer.print(emptyLine().toString)

    writer.print(comment(sectionFormatString.format("Imported Symbols")))
    externs.foreach(extern => writer.print(extern.toString))
    writer.print(emptyLine().toString)

    writer.print(comment(sectionFormatString.format("Functions")))
    functions.foreach(
      lines => {
        lines.foreach(
          line => writer.print(line.toString)
        )
        writer.print(emptyLine().toString)
      })

    writer.print(comment(sectionFormatString.format("Classes")))
    classes.foreach {
      classDef =>
        classDef.foreach {
          line => writer.print(line.toString)
            writer.print(emptyLine().toString)
        }
    }

    writer.print(comment(sectionFormatString.format("Text")))
    writer.print(section(AssemblySection.Text))
    text.foreach(writer.print)
    writer.print(emptyLine())

    writer.flush()
    writer.close()
  }
}

object AssemblyFileManager {
  def apply(writer: PrintWriter) = new AssemblyFileManager(writer)
}
