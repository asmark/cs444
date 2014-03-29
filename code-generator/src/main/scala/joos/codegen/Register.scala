package joos.codegen

import joos.core.Enumeration

class Register(val name: String, description: String = "") extends Register.Value {
  override def toString = {
    if (description.isEmpty) {
      name
    } else {
      s"${name}: ${description}"
    }
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
  final val Esp = this + new Register("esp", "Stack pointer")
  final val Ebp = this + new Register("ebp", "Frame pointer")
}
