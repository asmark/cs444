package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.types.PrimitiveType
import joos.codegen.generators.commonlib.ArithmeticOperations._
import joos.codegen.generators.commonlib.ComparisonOperations._
import joos.codegen.{AssemblyNamespace, AssemblyCodeGeneratorEnvironment, AssemblyFileManager}
import joos.semantic.isArraySuperType

package object commonlib {

  def AssemblyCommonLibraryEnvironment(namespace: AssemblyNamespace, sitBuilder: StaticDataManager): AssemblyCodeGeneratorEnvironment = {

    val assemblyManager = new AssemblyFileManager("_lib.s")

    def exportFunctions(functions: String*) {
      namespace.externs ++= functions
      assemblyManager.appendGlobal(functions: _*)
    }

    // -- Integer Operation Functions
    val integerOperations = Seq(addIntegers, subtractIntegers, multiplyIntegers, divideIntegers, moduloIntegers)
    // Export functions
    exportFunctions(integerOperations: _*)

    // Define functions
    assemblyManager.appendText(
      addInts ++ subInts ++ multInts ++ divInts ++ modInts: _*)

    // -- Comparison operations
    val comparisonOperations = Seq(
      compareAnd,
      compareOr,
      compareGreater,
      compareLess,
      compareEqual,
      compareNotEqual,
      compareLessEqual,
      compareGreaterEqual)
    // Export functions
    exportFunctions(comparisonOperations: _*)

    // Define functions
    assemblyManager.appendText(
      cmpAnd ++ cmpOr ++ cmpGt ++ cmpLt ++ cmpEq ++ cmpNe ++ cmpLe ++ cmpGe: _*
    )

    // For testing native method
    val nativeMethodAddOne = "NATIVEshengmin.BasicNativeMethod.nativeAddOne"
    assemblyManager.appendText(
      nativeMethodAddOne ::,
      add(Eax, 1),
      ret(),
      emptyLine
    )
    exportFunctions(nativeMethodAddOne)

    // Null check library function. Assumes operand is on stack
    val nullCheckOk = nextLabel("_null_check_ok")
    assemblyManager.appendText(
      :#("[BEGIN] Null Check Library Function"),
      nullCheck ::
    )
    assemblyManager.appendText(prologue(0): _*)
    assemblyManager.appendText(
      mov(Ebx, 0) :# "put null into ebx",
      cmp(Ebx, at(Ebp + 8)) :# "check if parameter passed is null",
      jne(nullCheckOk) :# "Skip to end if argument is not null",
      call(exceptionLabel) :# "throw exception if null",
      nullCheckOk ::,
      emptyLine
    )
    assemblyManager.appendText(epilogue: _*)
    assemblyManager.appendText(:#("[END] Null Check Library Function"), emptyLine)

    exportFunctions(nullCheck)

    PrimitiveType.values.foreach {
      primitive =>

      // Primitive ARRAY selector table
        val arraySelectorTableLabel = arrayPrefixLabel(selectorTableLabel(primitive.uniqueName))
        exportFunctions(arraySelectorTableLabel)
        assemblyManager.appendData(arraySelectorTableLabel ::)

        sitBuilder.orderedMethods.foreach {
          method =>
            if (isArraySuperType(method.typeDeclaration)) {
              assemblyManager.appendData(dd(method.uniqueName) :# s"${method.uniqueName} implemented by ${method.uniqueName}")
            } else {
              assemblyManager.appendData(dd(0) :# s"${method.uniqueName} not implemented by ${arrayPrefixLabel(primitive.uniqueName)}")
            }
        }
        assemblyManager.appendData(emptyLine)

        //  Primitive ARRAY subtype table
        val arraySubtypeTableLabel = arrayPrefixLabel(subtypeTableLabel(primitive.uniqueName))
        exportFunctions(arraySubtypeTableLabel)
        assemblyManager.appendData(arraySubtypeTableLabel ::)

        // Append subtypes of plain objects
        sitBuilder.orderedTypes.foreach {
          tipe =>
            if (isArraySuperType(tipe)) {
              assemblyManager.appendData(dd(1) :# s"${tipe.fullName}")
            } else {
              assemblyManager.appendData(dd(0) :# s"${tipe.fullName}")
            }
        }

        // Append subtypes of array objects
        sitBuilder.orderedTypes.foreach {
          tipe =>
            if (isArraySuperType(tipe)) {
              assemblyManager.appendData(dd(1) :# s"${arrayPrefixLabel(tipe.fullName)}")
            } else {
              assemblyManager.appendData(dd(0) :# s"${arrayPrefixLabel(tipe.fullName)}")
            }
        }

        // Append subtypes of other primitive array types
        PrimitiveType.values.foreach {
          other =>
            if (primitive == other) {
              assemblyManager.appendData(dd(1) :# s"${arrayPrefixLabel(other.uniqueName)}")
            } else {
              assemblyManager.appendData(dd(0) :# s"${arrayPrefixLabel(other.uniqueName)}")
            }
        }
        assemblyManager.appendData(emptyLine)
    }

    new AssemblyCodeGeneratorEnvironment(assemblyManager, namespace, sitBuilder)
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

  val nullCheck = "_lib_null_check"

}
