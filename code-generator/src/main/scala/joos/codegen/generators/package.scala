package joos.codegen

import joos.assemgen.Register._
import joos.assemgen._
import joos.core.DefaultUniqueIdGenerator


package object generators {
  val offsetPostFix = "_offset"

  def nextLabel(labelPrefix: String = "label") = labelPrefix + "_" + DefaultUniqueIdGenerator.nextId

  def prologue(frameSize: Int) = Seq(
    comment("[BEGIN] Function Prologue"),
    push(Ebp),
    mov(Ebp, Esp),
    sub(Esp, frameSize),
    push(Ebx),
    push(Ecx),
    push(Edx),
    push(Esi),
    push(Edi),
    comment("[END] Function Prologue"),
    emptyLine()
  )

  def epilogue = Seq(
    comment("[BEGIN] Function Epilogue"),
    pop(Edi),
    pop(Esi),
    pop(Edx),
    pop(Ecx),
    pop(Ebx),
    mov(Esp, Ebp),
    pop(Ebp),
    ret,
    comment("[END] Function Epilogue")
  )
}
