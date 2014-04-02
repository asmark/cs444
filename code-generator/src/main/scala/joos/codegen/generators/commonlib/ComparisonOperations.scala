package joos.codegen.generators.commonlib

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.generators._

object ComparisonOperations {

  private def binaryOperator(name: String, instructions: Seq[AssemblyLine]) = {
    Seq(
      :#(s"[BEGIN] ${name} Library Function"),
      (name ::)) ++
        prologue(0) ++
        Seq(
          mov(Eax, at(Ebp + 12)) :# "put left operand in eax",
          mov(Ebx, at(Ebp + 8)) :# "put right operand in ebx") ++
        instructions ++
        Seq(emptyLine) ++
        epilogue ++
        Seq(
          :#("[END] And Library Function"),
          emptyLine
        )
  }

  private def inequalityInstructions(name: String, jumpLabel: String, jumpInstruction: LabelReference => AssemblyLine) = {
    binaryOperator(
      name,
      Seq(
        cmp(Eax, Ebx),
        mov(Ebx, 1),
        jumpInstruction(jumpLabel),
        mov(Ebx, 0),
        (jumpLabel ::),
        mov(Eax, Ebx)
      ))
  }

  private[commonlib] val cmpAnd = binaryOperator(compareAnd, Seq(and(Eax, Ebx)))

  private[commonlib] val cmpOr = binaryOperator(compareOr, Seq(or(Eax, Ebx)))

  private[commonlib] val cmpGt = inequalityInstructions(compareGreater, nextLabel("jump_gt"), jg)

  private[commonlib] val cmpLt = inequalityInstructions(compareLess, nextLabel("jump_lt"), jl)

  private[commonlib] val cmpEq = inequalityInstructions(compareEqual, nextLabel("jump_eq"), je)

  private[commonlib] val cmpNe = inequalityInstructions(compareNotEqual, nextLabel("jump_ne"), jne)

  private[commonlib] val cmpLe = inequalityInstructions(compareLessEqual, nextLabel("jump_le"), jle)

  private[commonlib] val cmpGe = inequalityInstructions(compareGreaterEqual, nextLabel("jump_ge"), jge)


}
