package joos.assemgen

import java.io.PrintWriter

/**
 * Represents a reference to a label
 */
class LabelReference(val name: String) extends AssemblyExpression {
  override def write(writer: PrintWriter) {
    writer.print(name)
  }
}
