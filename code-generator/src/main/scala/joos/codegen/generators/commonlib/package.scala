package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.{AssemblyNamespace, AssemblyCodeGeneratorEnvironment, AssemblyFileManager}

package object commonlib {

  def AssemblyCommonLibraryEnvironment(namespace: AssemblyNamespace): AssemblyCodeGeneratorEnvironment = {

    val assemblyManager = new AssemblyFileManager("_lib.s")
    assemblyManager.appendGlobal(global(addIntegers))
    assemblyManager.appendText(addInts: _*)

    new AssemblyCodeGeneratorEnvironment(assemblyManager, namespace)
  }

  val addIntegers = "_lib_add_int"
  private val addInts = {
    Seq(
      comment("[BEGIN] Add Integer Library Function"),
      label(addIntegers)) ++
        prologue(0) ++
        Seq(
          mov(Eax, at(Ebp + 12), "put left operand in eax"),
          mov(Ebx, at(Ebp + 8), "put right operand in ebx"),
          add(Eax, Ebx, "add left and right and put answer in eax"),
          emptyLine
        ) ++
        epilogue ++
        Seq(
          comment("[END] Add Integer Library Function"),
          emptyLine
        )
  }

}
