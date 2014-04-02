package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.{SimpleNameExpression, ClassInstanceCreationExpression}
import joos.ast.declarations.{MethodDeclaration, FieldDeclaration, TypeDeclaration}
import joos.semantic._
import joos.codegen._
import joos.core.Logger
import scala.collection.mutable

class ClassInstanceCreationExpressionCodeGenerator(expression: ClassInstanceCreationExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  final val field_base_offset = 4
  var allInstanceFields = mutable.LinkedHashSet.empty[FieldDeclaration]

  private def getAllInstanceFields(typeDeclaration: TypeDeclaration): mutable.LinkedHashSet[FieldDeclaration] = {
    val result = mutable.LinkedHashSet.empty[FieldDeclaration]
    typeDeclaration.allSuperClasses.foreach(
      ancestor => {
        val instanceFields = ancestor.fieldMap.filter(pair => if (pair._2.isStatic) false else true)
        instanceFields.foreach(pair => result.add(pair._2))
      }
    )

    result
  }

  private def allocMem(typeDeclaration: TypeDeclaration) {
    allInstanceFields = getAllInstanceFields(typeDeclaration)
    val numFields = allInstanceFields.size

    val instanceSize = 4 * numFields + field_base_offset // Pointer to the class info table takes 4 bytes
    val typeInfoLabel = typeDeclaration.uniqueName
    appendText(
    #:("[BEG] Allocate memory for:" + expression.classType.toString)
    )

    appendText(
      mov(Eax, instanceSize) #:("Instance Size: " + instanceSize),
      call(labelReference("__malloc")),
      mov(Eax, labelReference(typeInfoLabel)) #:"Setting reference to the type info table"
    )

    val instanceFields = allInstanceFields.toIndexedSeq
    for (i <- 0 until instanceFields.size) {
      appendData(
        (instanceFields(i).uniqueName + offsetPostFix)::dd(field_base_offset + i * 4)
      )
    }

    appendText(
      #:("[END] Allocate memory for:" + expression.classType.toString)
    )
  }

  private def callConstructor(typeDeclaration: TypeDeclaration) {
    val compilationUnit = typeDeclaration.compilationUnit

    def matchConstructor(methodDeclaration: MethodDeclaration): Boolean = {
      if (expression.arguments.length != methodDeclaration.parameters.length) {
        return false
      }

      for (i <- 0 until expression.arguments.length) {
        if (!areEqual(expression.arguments(i).expressionType, methodDeclaration.parameters(i).declarationType)(compilationUnit)) {
          return false
        }
      }

      true
    }

    val constructor = typeDeclaration.constructorMap.values.find(matchConstructor)

    constructor match {
      case Some(target) => {
        // EAX should point to the appropriate location where the fields start
        appendText(
          #:("[BEG] Call constructor of " + typeDeclaration.fullName),
          push(Ecx),
          push(Edx),
          // The constructor is not expected to forward Eax, the code in callAllConstructors handles it
          call(labelReference(target.uniqueName)),
          pop(Edx),
          pop(Ecx),
          #:("[END] Call constructor of " + typeDeclaration.fullName)
        )
      }
      case _ => Logger.logWarning("Unable to find constructor")
    }
  }

  private def callAllConstructors(typeDeclaration: TypeDeclaration) {
    // Eax will be updated in the code below, save it here first
    appendText(push(Eax))

    typeDeclaration.allAncestors.foreach(
      ancestor => {
        callConstructor(ancestor)

        // Add offset
        val offset = ancestor.fieldMap.filter(pair => if (pair._2.isStatic) false else true).size * 4
        appendText(
          #: ("[BEGIN] Adding offset on constructor call: " + offset)
        )
        prologue(0)
        appendText(
          mov(Ebx, toExpression(offset)),
          add(Eax, Ebx)
        )
        epilogue(0)
        appendText(
          #: ("[END] Adding offset on constructor call: " + offset)
        )
      }
    )

    callConstructor(typeDeclaration)

    // Get back the old value of eax
    appendText(push(Eax))
  }

  // Returns the reference to the object in EAX
  override def generate() {
    expression.arguments.foreach(_.generate())

    val typeDeclaration = expression.classType.declaration
    assert(typeDeclaration != null)

    appendText(
      #:(s"[BEG] new ${expression.classType.standardName}")
    )

    allocMem(typeDeclaration)
    // Eax should has the reference to the new object
    callAllConstructors(typeDeclaration)

    appendText(
      #:(s"[END] new ${expression.classType.standardName}")
    )
  }
}