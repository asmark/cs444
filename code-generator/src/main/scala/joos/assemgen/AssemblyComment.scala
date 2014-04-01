package joos.assemgen

import java.io.PrintWriter

class AssemblyComment(val text: String) extends AssemblyLine {
  def #:(instruction: AssemblyInstruction): AssemblyInstruction = {
    new AssemblyInstruction(instruction.name, instruction.operands, Some(this))
  }

  def #:(label: AssemblyLabel): AssemblyLabel = {
    new AssemblyLabel(label.name, Some(this))
  }

  override def write(writer: PrintWriter) {
    writer.print("; ")
    writer.print(text)
  }
}
