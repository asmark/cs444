package joos.codegen

import joos.assemgen.Register._
import joos.assemgen._
import joos.core.{Logger, DefaultUniqueIdGenerator}
import joos.ast.declarations.FieldDeclaration
import joos.ast.types.{SimpleType, PrimitiveType, ArrayType}


package object generators {
  val offsetPostFix = "_offset"
  val subTypeTable = "_subTypeTable"

  def nextLabel(labelPrefix: String = "label") = labelPrefix + "_" + DefaultUniqueIdGenerator.nextId

  def prologue(frameSize: Int) = Seq(
    #: ("[BEGIN] Function Prologue"),
    push(Ebp),
    mov(Ebp, Esp),
    sub(Esp, frameSize),
    push(Ebx),
    push(Esi),
    push(Edi),
    #: ("[END] Function Prologue"),
    emptyLine
  )

  def epilogue = Seq(
    #: ("[BEGIN] Function Epilogue"),
    pop(Edi),
    pop(Esi),
    pop(Ebx),
    mov(Esp, Ebp),
    pop(Ebp),
    ret(),
    #: ("[END] Function Epilogue")
  )

  // The default value will be written to EDX
  def initDefault(fieldDeclaration: FieldDeclaration) = {
    fieldDeclaration.declarationType match {
      case ArrayType(_,_) => {
        Seq(
          mov(Edx, toExpression(0)) #:"Init default ArrayType"
        )
      }
      case PrimitiveType.IntegerType => {
        Seq(
          mov(Edx, toExpression(0)) #:"Init default IntegerType"
        )
      }
      case PrimitiveType.BooleanType => {
        Seq(
          mov(Edx, toExpression(0)) #:"Init default BooleanType"
        )
      }
      case PrimitiveType.CharType => {
        Seq(
          mov(Edx, toExpression(0)) #:"Init default CharType"
        )
      }
      case PrimitiveType.ShortType => {
        Seq(
          mov(Edx, toExpression(0)) #:"Init default Short"
        )
      }
      case PrimitiveType.NullType | PrimitiveType.VoidType => {
        Logger.logError("Field should not be of type null of void")
      }
      case SimpleType(_) => {
        Seq(
          mov(Edx, toExpression(0)) #:"Init default SimpleType"
        )
      }
    }
  }
}
