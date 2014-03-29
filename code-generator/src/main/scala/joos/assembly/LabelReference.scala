package joos.assembly

import java.io.PrintWriter

/**
 * Represents a reference to a label
 */
class LabelReference(val name: String) extends AssemblyExpression {
  override def write(writer: PrintWriter) {
    writer.print(' ')
    writer.print(name)
    writer.print(' ')
  }
}
