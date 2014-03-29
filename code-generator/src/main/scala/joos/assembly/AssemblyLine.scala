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

//
//  /**
//   * Label for the assembly line
//   */
//  def label: Option[String] = None
//
//  /**
//   * Comment for the assembly line
//   */
//  def comment: Option[String] = None
//
//  /**
//   * Instruction for the assembly line
//   */
//  def instruction: Option[Instruction] = None
//
//  /**
//   * Writes this assembly line to the {{writer}}
//   */
//  override def write(writer: PrintWriter) {
//    label match {
//      case None =>
//      case Some(label) =>
//        writer.print(label)
//        writer.print(':')
//    }
//
//    instruction match {
//      case None =>
//      case Some(instruction) =>
//        writer.print("    ")
//        instruction.write(writer)
//    }
//
//    comment match {
//      case None =>
//      case Some(comment) =>
//        writer.print("    ;")
//        writer.print(comment)
//    }
//
//    writer.println()
//  }
}
