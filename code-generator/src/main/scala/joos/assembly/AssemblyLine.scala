package joos.assembly

import java.io.{ByteArrayOutputStream, PrintWriter}
import joos.core.Writable

/**
 * Represents a line in assembly code
 */
trait AssemblyLine extends Writable {
  override def toString = {
    val stream = new ByteArrayOutputStream()
    val writer = new PrintWriter(stream)
    write(writer)
    writer.flush()
    stream.toString
  }

  override def write(writer: PrintWriter) {
    writeContent(writer)
    writer.println()
  }

  /**
   * Writes the content of this line to the writer
   */
  protected def writeContent(writer: PrintWriter): Unit
}
