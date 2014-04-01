package joos.codegen.generators.commonlib

import joos.assemgen._
import joos.codegen.generators._
import joos.assemgen.Register._

object ComparisonOperations {

  private[commonlib] val cmpAnd = {
    Seq(
      #:("[BEGIN] And Library Function"),
      (compareAnd::)) ++
        prologue(0) ++
        Seq(
          mov(Eax, at(Ebp + 12)) #: "put left operand in eax",
          mov(Ebx, at(Ebp + 8)) #: "put right operand in ebx",
          and(Eax, Ebx) #: "and left and right and put answer in eax",
          emptyLine
        ) ++
        epilogue ++
        Seq(
          #:("[END] And Library Function"),
          emptyLine
        )
  }

  private[commonlib] val cmpOr = {
    Seq(
      #:("[BEGIN] And Library Function"),
      (compareAnd::)) ++
        prologue(0) ++
        Seq(
          mov(Eax, at(Ebp + 12)) #: "put left operand in eax",
          mov(Ebx, at(Ebp + 8)) #: "put right operand in ebx",
          and(Eax, Ebx) #: "and left and right and put answer in eax",
          emptyLine
        ) ++
        epilogue ++
        Seq(
          #:("[END] And Library Function"),
          emptyLine
        )
  }

}
