package joos.codegen

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.declarations.{MethodDeclaration, FieldDeclaration, TypeDeclaration}
import joos.ast.expressions.SimpleNameExpression
import joos.ast.types.{SimpleType, PrimitiveType, ArrayType}
import joos.core.{Logger, DefaultUniqueIdGenerator}


package object generators {
  val mallocLabel = "__malloc"
  val exceptionLabel = "__exception"
  val offsetPostFix = "_offset"

  val FieldOffset = 8
  val ParameterOffset = 4

  def nextLabel(labelPrefix: String = "label") = labelPrefix + "_" + DefaultUniqueIdGenerator.nextId

  def selectorTableLabel(tipe: TypeDeclaration) = s"selector_table_${tipe.uniqueName}"

  def subtypeTableLabel(tipe: TypeDeclaration) = s"subtype_table_${tipe.uniqueName}"

  def mallocTypeLabel(tipe: TypeDeclaration) = s"malloc_${tipe.uniqueName}"

  def getLocalVariableInstruction(variable: SimpleNameExpression, method: MethodDeclaration, register: Register) = {
    if (method.isParameter(variable)) {
      val slot = method.getParameterSlot(variable)
      add(register, (slot*4) + ParameterOffset)
    } else {
      assert(method.isLocal(variable))
      val slot = method.getLocalSlot(variable)
      sub(register, slot*4)
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

  def initField(fieldDeclaration: FieldDeclaration) (implicit environment: AssemblyCodeGeneratorEnvironment) {
    val codeGenerator = new FieldDeclarationCodeGenerator(fieldDeclaration)
    codeGenerator.generate()
  }

  // TODO: this method should be deprecated and it not quite
  def initDefault(fieldDeclaration: FieldDeclaration) {
    fieldDeclaration.declarationType match {
      case ArrayType(_, _) => {
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
