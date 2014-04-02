package joos.assemgen

import java.io.PrintWriter
import joos.core.Writable

class AssemblyLabel(val name: String, next: Option[AssemblyLine]) extends AssemblyLine {
  override def write(writer: PrintWriter) {
    writer.print(name)
    writer.print(':')

    next match {
      case None =>
      case Some(next) =>
        writer.print("    ")
        next.write(writer)
    }
  }

  override def :#(comment: String): AssemblyLine = {
    next match {
      case Some(line) => new AssemblyLabel(name, Some(line :# comment))
      case None => new AssemblyLabel(name, Some(new AssemblyComment(comment)))
    }
  }
}
