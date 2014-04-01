package joos.codegen

import joos.assemgen.Register._
import joos.assemgen._
import joos.core.DefaultUniqueIdGenerator


package object generators {
  val offsetPostFix = "_offset"

  def nextLabel(labelPrefix: String = "label") = labelPrefix + "_" + DefaultUniqueIdGenerator.nextId

  def prologue(frameSize: Int) = Seq(
    #: ("[BEGIN] Function Prologue"),
    push(Ebp),
    mov(Ebp, Esp),
    sub(Esp, frameSize),
    push(Ebx),
    push(Ecx),
    push(Edx),
    push(Esi),
    push(Edi),
    #: ("[END] Function Prologue"),
    emptyLine
  )

  def epilogue = Seq(
    #: ("[BEGIN] Function Epilogue"),
    pop(Edi),
    pop(Esi),
    pop(Edx),
    pop(Ecx),
    pop(Ebx),
    mov(Esp, Ebp),
    pop(Ebp),
    ret(),
    #: ("[END] Function Epilogue")
  )
}
