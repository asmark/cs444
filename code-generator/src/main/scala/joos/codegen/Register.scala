package joos.codegen

import java.io.PrintWriter
import joos.core.Enumeration

class Register(val name: String)
    extends Register.Value
    with AssemblyExpression {

  override def write(writer: PrintWriter) {
    writer.print(name)
  }
}

object Register extends Enumeration {
  type T = Register

  final val Eax = this + new Register("eax")
  final val Ebx = this + new Register("ebx")
  final val Ecx = this + new Register("ecx")
  final val Edx = this + new Register("edx")
  final val Exi = this + new Register("exi")
  final val Edi = this + new Register("edi")
  /**
   * Stack pointer
   */
  final val Esp = this + new Register("esp")
  /**
   * Frame pointer
   */
  final val Ebp = this + new Register("ebp")
}
