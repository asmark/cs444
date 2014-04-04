package joos.codegen

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.declarations.{MethodDeclaration, TypeDeclaration}
import joos.ast.expressions.SimpleNameExpression
import joos.core.DefaultUniqueIdGenerator


package object generators {
  final val exceptionLabel = "__exception"
  final val mallocLabel = "__malloc"

  final val FieldOffset = 8
  final val SelectorTableOffset = 0
  final val SubtypeTableOffset = 4
  final val ParameterOffset = 4
  final val ArrayLengthOffset = 8
  final val ArrayFirstElementOffset = ArrayLengthOffset + 4

  def nextLabel(labelPrefix: String = "label") = labelPrefix + "_" + DefaultUniqueIdGenerator.nextId

  def arrayPrefixLabel(suffix: String) = s"array_${suffix}"

  def selectorTableLabel(name: String): String = s"selector_table_${name}"
  def selectorTableLabel(tipe: TypeDeclaration): String = selectorTableLabel(tipe.uniqueName)

  def subtypeTableLabel(name: String): String = s"subtype_table_${name}"
  def subtypeTableLabel(tipe: TypeDeclaration): String = subtypeTableLabel(tipe.uniqueName)

  def mallocTypeLabel(tipe: TypeDeclaration) = s"malloc_${tipe.uniqueName}"

  def classTableLabel(tipe: TypeDeclaration) = s"class_table_${tipe.uniqueName}"

  def getLocalVariableInstruction(variable: SimpleNameExpression, method: MethodDeclaration, register: Register) = {
    if (method.isParameter(variable)) {
      val slot = method.getParameterSlot(variable)
      if (method.isConstructor) {
        add(register, ((slot+1) * 4) + ParameterOffset)
      } else {
        add(register, ((slot+1) * 4) + ParameterOffset)
      }
    } else {
      assert(method.isLocal(variable))
      val slot = method.getLocalSlot(variable)
      sub(register, slot * 4)
    }
  }

  def prologue(frameSize: Int) = Seq(
    :#("[BEGIN] Function Prologue"),
    push(Ebp),
    mov(Ebp, Esp),
    sub(Esp, frameSize),
    push(Ebx),
    push(Esi),
    push(Edi),
    :#("[END] Function Prologue"),
    emptyLine
  )

  def epilogue = Seq(
    :#("[BEGIN] Function Epilogue"),
    pop(Edi),
    pop(Esi),
    pop(Ebx),
    mov(Esp, Ebp),
    pop(Ebp),
    ret(),
    :#("[END] Function Epilogue")
  )
}
