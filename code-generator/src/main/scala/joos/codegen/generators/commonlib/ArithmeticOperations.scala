package joos.codegen.generators.commonlib

import joos.assemgen._
import joos.codegen.generators._
import joos.assemgen.Register._

object ArithmeticOperations {

  private[commonlib] val addInts = {
    Seq(
      #:("[BEGIN] Add Integer Library Function"),
      (addIntegers::)) ++
        prologue(0) ++
        Seq(
          mov(Eax, at(Ebp + 12)) :# "put left operand in eax",
          mov(Ebx, at(Ebp + 8)) :# "put right operand in ebx",
          add(Eax, Ebx) :# "add left and right and put answer in eax",
          emptyLine
        ) ++
        epilogue ++
        Seq(
          #:("[END] Add Integer Library Function"),
          emptyLine
        )
  }


  private[commonlib] val subInts = {
    Seq(
      #:("[BEGIN] Subtract Integer Library Function"),
      (subtractIntegers::)) ++
        prologue(0) ++
        Seq(
          mov(Eax, at(Ebp + 12)) :# "put left operand in eax",
          mov(Ebx, at(Ebp + 8)) :# "put right operand in ebx",
          sub(Eax, Ebx) :# "subtract left and right and put answer in eax",
          emptyLine
        ) ++
        epilogue ++
        Seq(
          #:("[END] Subtract Integer Library Function"),
          emptyLine
        )
  }


  private[commonlib] val multInts = {
    Seq(
      #:("[BEGIN] Multiply Integer Library Function"),
      (multiplyIntegers::)) ++
        prologue(0) ++
        Seq(
          mov(Eax, at(Ebp + 12)) :# "put left operand in eax",
          mov(Ebx, at(Ebp + 8)) :# "put right operand in ebx",
          imul(Eax, Ebx) :# "multiply left and right and put answer in eax",
          emptyLine
        ) ++
        epilogue ++
        Seq(
          #:("[END] Multiply Integer Library Function"),
          emptyLine
        )
  }

  private[commonlib] val divInts = {
    val beginDivide = nextLabel(s"begin_divide")
    Seq(
      #:("[BEGIN] Divide Integer Library Function"),
      (divideIntegers::)) ++
        prologue(0) ++
        Seq(
          mov(Edx, 0) :# "Set edx as 0 for signed comparisons",
          mov(Eax, at(Ebp + 12)) :# "put left operand in eax",
          cmp(Eax, Edx) :# "Compare left operand to be greater than zero",
          jge(beginDivide) :# "If left operand is positive, skip to division",
          mov(Edx, -1) :# "Else, set edx to -1",
          beginDivide ::,
          mov(Ebx, at(Ebp + 8)) :# "put right operand in ebx",
          idiv(Ebx) :# "divide left and right and put answer in eax",
          emptyLine
        ) ++
        epilogue ++
        Seq(
          #:("[END] Divide Integer Library Function"),
          emptyLine
        )
  }

  private[commonlib] val modInts = {
    val beginModulo = nextLabel(s"begin_modulo")
    Seq(
      #:("[BEGIN] Modulo Integer Library Function"),
      (moduloIntegers::)) ++
        prologue(0) ++
        Seq(
          mov(Edx, 0) :# "Set edx as 0 for signed comparisons",
          mov(Eax, at(Ebp + 12)) :# "put left operand in eax",
          cmp(Eax, Edx) :# "Compare left operand to be greater than zero",
          jge(beginModulo) :# "If left operand is positive, skip to division",
          mov(Edx, -1) :# "Else, set edx to -1",
          (beginModulo ::),
          mov(Ebx, at(Ebp + 8)) :# "put right operand in ebx",
          idiv(Ebx) :# "divide left and right and put answer in eax",
          mov(Eax, Edx) :# "put the remainder in eax",
          emptyLine
        ) ++
        epilogue ++
        Seq(
          #:("[END] Modulo Integer Library Function"),
          emptyLine
        )
  }



}
