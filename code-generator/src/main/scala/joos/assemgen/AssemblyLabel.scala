package joos.assemgen

import java.io.PrintWriter
import joos.core.Writable

class AssemblyLabel(val name: String, next: Option[Writable]) extends AssemblyLine {
  override def write(writer: PrintWriter) {
    writer.print(name)
    writer.print(':')

    next match {
      case None => writer.println()
      case Some(next) =>
        writer.print("    ")
        next.write(writer)
    }
  }
}
