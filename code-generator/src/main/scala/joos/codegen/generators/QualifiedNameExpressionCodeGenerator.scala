package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.NameClassification._
import joos.ast.expressions.QualifiedNameExpression
import joos.ast.types.{ArrayType, SimpleType}
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.codegen.generators.commonlib._
import joos.core.Logger

class QualifiedNameExpressionCodeGenerator(expression: QualifiedNameExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {

    expression.nameClassification match {

      case InstanceFieldName => {

        val fieldOwner = expression.qualifier.expressionType match {
          case simple: SimpleType => simple.declaration
          case array: ArrayType =>
            expression.qualifier.generate()
            appendText(
              :# ("Access Array.length"),
              lea(Edx, at(Eax + ArrayLengthOffset)),
              mov(Eax, at(Edx))
            )
            return
        }

        // Prefix type returned in Eax
        expression.qualifier.generate()
        appendText(
          push(Eax) :# "Push prefix type argument for null check",
          call(nullCheck) :# "Call null check on prefix object",
          pop(Eax) :# "Pop prefix object back into eax",
          emptyLine
        )

        val slot = fieldOwner.getFieldSlot(expression.name)
        appendText(
          :#(s"[BEGIN] Access qualified instance field ${expression}"),
          #>,
          mov(Edx, Eax) :# "Field Access. Move instance into edx",
          add(Edx, slot * 4 + FieldOffset) :# s"lvalue of field ${expression.standardName} is at offset ${slot * 4 + FieldOffset}",
          mov(Eax, at(Edx)) :# s"Retrieve field ${expression.standardName}",
          #<,
          :#(s"[END] Access qualified instance field ${expression}")
        )
      }

      case StaticFieldName => {
        val fieldOwner = expression.qualifier.expressionType match {
          case simple: SimpleType => simple.declaration
        }

        val staticField = fieldOwner.containedFields(expression.name)
        appendText(
          :#(s"[BEGIN] Access qualified static field ${expression}"),
          #>,
          movdw(Edx, staticField.uniqueName) :# s"Move location of ${staticField.declarationName} storage to eax",
          mov(Eax, at(Edx)) :# s"Retrieve field",
          #<,
          :#(s"[END] Access qualified static field ${expression}")
        )
      }

      case e =>
        Logger.logWarning(s"No support for accessing ${expression} of classification ${e}")
    }

  }

}
