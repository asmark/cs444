package joos.core

import java.io.PrintWriter

/**
 * Represents something that can be output to a writer
 */
trait Writable {
  def write(writer: PrintWriter): Unit
}
