package joos.codegen

import joos.assemgen.Register._
import joos.assemgen._
import joos.core.DefaultUniqueIdGenerator
import joos.ast.declarations.TypeDeclaration


package object generators {
  val mallocLabel = "__malloc"
  val offsetPostFix = "_offset"

  val FieldOffset = 8

  def nextLabel(labelPrefix: String = "label") = labelPrefix + "_" + DefaultUniqueIdGenerator.nextId
  def objectInfoTableLabel(tipe: TypeDeclaration) = s"object_info_${tipe.uniqueName}"
  def selectorTableLabel(tipe: TypeDeclaration) = s"selector_table_${tipe.uniqueName}"
  def subtypeTableLabel(tipe: TypeDeclaration) = s"subtype_table_${tipe.uniqueName}"
  def mallocTypeLabel(tipe: TypeDeclaration) = s"malloc_${tipe.uniqueName}"

  def prologue(frameSize: Int) = Seq(
    :# ("[BEGIN] Function Prologue"),
    push(Ebp),
    mov(Ebp, Esp),
    sub(Esp, frameSize),
    push(Ebx),
    push(Esi),
    push(Edi),
    :# ("[END] Function Prologue"),
    emptyLine
  )

  def epilogue = Seq(
    :# ("[BEGIN] Function Epilogue"),
    pop(Edi),
    pop(Esi),
    pop(Ebx),
    mov(Esp, Ebp),
    pop(Ebp),
    ret(),
    :# ("[END] Function Epilogue")
  )
}
