package joos.codegen

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.declarations.FieldDeclaration
import joos.ast.declarations.TypeDeclaration
import joos.ast.types.{SimpleType, PrimitiveType, ArrayType}
import joos.core.{Logger, DefaultUniqueIdGenerator}


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

  // The default value will be written to EDX
  def initDefault(fieldDeclaration: FieldDeclaration) = {
    fieldDeclaration.declarationType match {
      case ArrayType(_,_) => {
        Seq(
          mov(Edx, 0) :# "Init default ArrayType"
        )
      }
      case PrimitiveType.IntegerType => {
        Seq(
          mov(Edx, 0) :# "Init default IntegerType"
        )
      }
      case PrimitiveType.BooleanType => {
        Seq(
          mov(Edx, 0) :# "Init default BooleanType"
        )
      }
      case PrimitiveType.CharType => {
        Seq(
          mov(Edx, 0) :# "Init default CharType"
        )
      }
      case PrimitiveType.ShortType => {
        Seq(
          mov(Edx, 0) :# "Init default Short"
        )
      }
      case PrimitiveType.NullType | PrimitiveType.VoidType => {
        Logger.logError("Field should not be of type null of void")
      }
      case SimpleType(_) => {
        Seq(
          mov(Edx, 0) :# "Init default SimpleType"
        )
      }
    }
  }
}
