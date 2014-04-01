package joos.assemgen

import java.io.PrintWriter

/**
 * Invoking an instruction
 * @param name  Name of the instruction
 */
class AssemblyInstruction(
    val name: String,
    val operands: Seq[AssemblyExpression] = Seq(),
    val comment: Option[AssemblyComment] = None) extends AssemblyLine {

  def ::(label: String): AssemblyLabel = {
    new AssemblyLabel(label, Some(this))
  }

  override def write(writer: PrintWriter) {
    writer.print(name)
    writer.print(' ')

    operands.foldLeft(true) {
      (isFirst, operand) =>
        if (isFirst) {
          operand.write(writer)
        } else {
          writer.print(", ")
          operand.write(writer)
        }
        false
    }

    comment match {
      case None =>
      case Some(comment) =>
        writer.print("    ")
        comment.write(writer)
    }
  }
}
