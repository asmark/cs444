package joos.codegen

import java.io.PrintWriter
import joos.assemgen.{EmptyLine, Indentation, AssemblyLine}

class AssemblyCodeWriter(writer: PrintWriter) {
  private[this] var indentation = 0

  def write(lines: AssemblyLine*): this.type = {
    for (line <- lines) {
      line match {
        case line: EmptyLine => writer.println()
        case line: Indentation => indentation += line.amount
        case _ =>
          for (i <- 0 until indentation) {
            writer.print(' ')
          }
          line.write(writer)
          writer.println()
      }
    }
    this
  }

  def flush(): this.type = {
    writer.flush()
    this
  }

  def close(): this.type = {
    writer.close()
    this
  }
}
