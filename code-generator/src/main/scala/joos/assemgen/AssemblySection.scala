package joos.assemgen

import java.io.PrintWriter
import joos.core.Enumeration

class AssemblySection(val name: String)
    extends AssemblySection.Value with AssemblyExpression {

  override def write(writer: PrintWriter) {
    writer.print('.')
    writer.print(name)
  }
}

object AssemblySection extends Enumeration {
  type T = AssemblySection

  final val Data = this + new AssemblySection("data")
  final val Text = this + new AssemblySection("text")
}
