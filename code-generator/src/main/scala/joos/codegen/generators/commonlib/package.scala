package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.generators.commonlib.ArithmeticOperations._
import joos.codegen.{AssemblyNamespace, AssemblyCodeGeneratorEnvironment, AssemblyFileManager}

package object commonlib {

  def AssemblyCommonLibraryEnvironment(namespace: AssemblyNamespace): AssemblyCodeGeneratorEnvironment = {

    val assemblyManager = new AssemblyFileManager("_lib.s")

    // Extern all library functions
    namespace.externs ++= integerOperations

    // Global all library functions
    assemblyManager.appendGlobal(integerOperations :_*)

    // Define all library functions
    assemblyManager.appendText(
      addInts ++ subInts ++ multInts ++ divInts ++ modInts: _*)

    new AssemblyCodeGeneratorEnvironment(assemblyManager, namespace)
  }

  val addIntegers = "_lib_add_ints"
  val subtractIntegers = "_lib_sub_ints"
  val multiplyIntegers = "_lib_mult_ints"
  val divideIntegers = "_lib_divide_ints"
  val moduloIntegers = "_lib_mod_ints"

  val integerOperations = Seq(addIntegers, subtractIntegers, multiplyIntegers, divideIntegers, moduloIntegers)
}
