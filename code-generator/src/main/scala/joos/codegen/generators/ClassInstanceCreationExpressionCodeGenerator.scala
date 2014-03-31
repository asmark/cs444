package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.ClassInstanceCreationExpression
import joos.ast.declarations.TypeDeclaration
import joos.semantic._
import joos.codegen._
import joos.core.Logger

class ClassInstanceCreationExpressionCodeGenerator(expression: ClassInstanceCreationExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  private def allocMem(typeDeclaration: TypeDeclaration) {
    val numFields = typeDeclaration.containedFields.size
    val instanceSize = 4 * (numFields + 1) // Pointer to the class info table takes 4 bytes
    val typeInfoLabel = typeDeclaration.uniqueName

    appendText(
      comment("[BEG] Allocate memory for:" + expression.classType.toString),
    )

    appendText(
      mov(Eax, instanceSize, "Instance Size: " + instanceSize),
      call(labelReference("__malloc")),
      mov(Eax, labelReference(typeInfoLabel))
    )

    appendText(
      comment("[END] Allocate memory for:" + expression.classType.toString)
    )
  }

  private def callConstructor(typeDeclaration: TypeDeclaration) {
    val compilationUnit = typeDeclaration.compilationUnit

    val constructor = typeDeclaration.constructorMap.values.find(
      methodDeclaration => {
        if (expression.arguments.length != methodDeclaration.parameters.length)
          false
        for (i <- 0 until expression.arguments.length) {
          if (!areEqual(expression.arguments(i).expressionType, methodDeclaration.parameters(i).declarationType)(compilationUnit))
            false
        }
        true
      }
    )

    constructor match {
      case Some(constructor) => {
        comment("[BEG] Call constructor")
        appendText(
          push(Ecx),
          // Ecx now should point to the first field in the object
          mov(Ecx, Eax)
        )
        executeProcedure(constructor)
        appendText(pop(Ecx))
        comment("[END] Call constructor")
      }
      case _ => Logger.logWarning("Unable to find constructor")
    }
  }

  // Returns the reference to the object in EAX
  override def generate() {
    expression.arguments.foreach(_.generate())

    val typeDeclaration = expression.classType.declaration
    assert(typeDeclaration != null)

    appendText(
      comment(s"[BEG] new ${expression.classType.standardName}")
    )

    allocMem(typeDeclaration)
    callConstructor(typeDeclaration)

    appendText(
      comment(s"[END] new ${expression.classType.standardName}")
    )
  }
}