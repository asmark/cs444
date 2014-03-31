package joos.core

import java.io.{ByteArrayOutputStream, PrintWriter}

/**
 * Represents something that can be output to a writer
 */
trait Writable {
  def write(writer: PrintWriter)

  override def toString() = {
    val stream = new ByteArrayOutputStream()
    val writer = new PrintWriter(stream)
    write(writer)
    writer.flush()
    stream.toString
  }
}
