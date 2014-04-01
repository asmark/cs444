package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.generators.commonlib.ArithmeticOperations._
import joos.codegen.generators.commonlib.ComparisonOperations._
import joos.codegen.{AssemblyNamespace, AssemblyCodeGeneratorEnvironment, AssemblyFileManager}

package object commonlib {

  def AssemblyCommonLibraryEnvironment(namespace: AssemblyNamespace): AssemblyCodeGeneratorEnvironment = {

    val assemblyManager = new AssemblyFileManager("_lib.s")

    def exportFunctions(functions: Seq[String]) {
      namespace.externs ++= functions
      assemblyManager.appendGlobal(functions :_*)
    }

    // -- Integer Operation Functions
    val integerOperations = Seq(addIntegers, subtractIntegers, multiplyIntegers, divideIntegers, moduloIntegers)
    // Export functions
    exportFunctions(integerOperations)

    // Define functions
    assemblyManager.appendText(
      addInts ++ subInts ++ multInts ++ divInts ++ modInts: _*)

    // -- Comparison operations
    val comparisonOperations = Seq(compareAnd, compareOr, compareGreater, compareLess, compareEqual, compareNotEqual, compareLessEqual, compareGreaterEqual)
    // Export functions
    exportFunctions(comparisonOperations)

    // Define functions
    assemblyManager.appendText(
      cmpAnd ++ cmpOr ++ cmpGt ++ cmpLt ++ cmpEq ++ cmpNe ++ cmpLe ++ cmpGe : _*
    )


    new AssemblyCodeGeneratorEnvironment(assemblyManager, namespace)
  }

  val addIntegers = "_lib_add_ints"
  val subtractIntegers = "_lib_sub_ints"
  val multiplyIntegers = "_lib_mult_ints"
  val divideIntegers = "_lib_divide_ints"
  val moduloIntegers = "_lib_mod_ints"

  val compareAnd = "_lib_cmp_and"
  val compareOr = "_lib_cmp_or"
  val compareGreater = "_lib_cmp_gt"
  val compareLess = "_lib_cmp_lt"
  val compareEqual = "_lib_cmp_eq"
  val compareNotEqual = "_lib_cmp_ne"
  val compareLessEqual = "_lib_cmp_le"
  val compareGreaterEqual = "_lib_cmp_ge"
}
