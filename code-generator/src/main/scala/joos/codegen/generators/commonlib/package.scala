package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.{AssemblyNamespace, AssemblyCodeGeneratorEnvironment, AssemblyFileManager}

package object commonlib {

  def AssemblyCommonLibraryEnvironment(namespace: AssemblyNamespace): AssemblyCodeGeneratorEnvironment = {

    val assemblyManager = new AssemblyFileManager("_lib.s")

    // Extern all library functions
    namespace.externs += addIntegers

    // Define all library functions
    assemblyManager.appendGlobal(addIntegers)
    assemblyManager.appendText(addInts: _*)

    new AssemblyCodeGeneratorEnvironment(assemblyManager, namespace)
  }

  val addIntegers = "_lib_add_int"
  private val addInts = {
    Seq(
      #: ("[BEGIN] Add Integer Library Function"),
      (addIntegers::)) ++
        prologue(0) ++
        Seq(
          mov(Eax, at(Ebp + 12)) #: "put left operand in eax",
          mov(Ebx, at(Ebp + 8)) #: "put right operand in ebx",
          add(Eax, Ebx) #: "add left and right and put answer in eax",
          emptyLine()
        ) ++
        epilogue ++
        Seq(
          #: ("[END] Add Integer Library Function"),
          emptyLine()
        )
  }

}
