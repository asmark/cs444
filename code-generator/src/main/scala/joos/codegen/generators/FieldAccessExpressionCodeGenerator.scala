package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.FieldAccessExpression
import joos.ast.types.{SimpleType, ArrayType}
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.codegen.generators.commonlib._
import joos.ast.NameClassification._

class FieldAccessExpressionCodeGenerator(expression: FieldAccessExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    require(expression.declaration != null)

    expression.identifier.nameClassification match {
      case InstanceFieldName => accessInstanceField()
      case StaticFieldName => accessStaticField()
    }

  }

  private def accessInstanceField() {
    expression.expression.generate()
    appendText(
      push(Eax) :# "Push prefix type argument for null check",
      call(nullCheck) :# "Call null check on prefix object",
      pop(Eax) :# "Pop prefix object back into eax",
      emptyLine
    )

    if (expression.declaration eq ArrayType.Length) {
      appendText(
        :#("Access Array.length"),
        lea(Edx, at(Eax + ArrayLengthOffset)),
        mov(Eax, at(Edx))
      )
      return
    }



    val prefixType = expression.expression.expressionType.declaration
    val slot = prefixType.getFieldSlot(expression.identifier)
    appendText(
      :#(s"[BEGIN] Access qualified instance field ${expression}"),
      mov(Edx, Eax) :# "Field Access. Move instance into edx",
      add(Edx, slot * 4 + FieldOffset) :# s"lvalue of field ${expression.identifier} is at offset ${slot * 4 + FieldOffset}",
      mov(Eax, at(Edx)) :# s"Retrieve field ${expression.identifier}",
      :#(s"[END] Access qualified instance field ${expression}")
    )
  }

  private def accessStaticField() {
    val fieldOwner = expression.expression.expressionType.declaration
    val staticField = fieldOwner.containedFields(expression.identifier)
    appendText(
      :#(s"[BEGIN] Access qualified static field ${expression}"),
      #>,
      movdw(Edx, staticField.uniqueName) :# s"Move location of ${staticField.declarationName} storage to eax",
      mov(Eax, at(Edx)) :# s"Retrieve field",
      #<,
      :#(s"[END] Access qualified static field ${expression}")
    )
  }

}
