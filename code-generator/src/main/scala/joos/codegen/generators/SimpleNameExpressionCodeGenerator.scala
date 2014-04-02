package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.NameClassification._
import joos.ast.expressions.SimpleNameExpression
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.core.Logger
import joos.ast.types.ArrayType

class SimpleNameExpressionCodeGenerator(expression: SimpleNameExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {

    expression.nameClassification match {

      case LocalVariableName => {
//        val slot = environment.methodEnvironment.getVariableSlot(expression)
        appendText(
          mov(Edx, Ebp) :# "Local Variable access. Move ebp into edx",
          //          sub(Edx, slot * 4) :# s"Retrieve lvalue address of variable ${expression.standardName}",
          getLocalVariableInstruction(expression, environment.methodEnvironment, Edx)
              :# s"Retrieve lvalue address of variable ${expression.standardName}",
          mov(Eax, at(Edx)) :# s"Retrieve variable ${expression.standardName}"
        )
      }

      case InstanceFieldName => {

        val slot = environment.typeEnvironment.getFieldSlot(expression)
        // Assume Ecx is pointer to this
        appendText(
          mov(Edx, Ecx) :# "Field Access. Move this into edx",
          add(Edx, slot * 4 + FieldOffset) :# s"lvalue of field ${expression.standardName} is at offset ${slot * 4 + FieldOffset}",
          mov(Eax, at(Edx)) :# s"Retrieve field ${expression.standardName}"
        )
      }

      case x =>
        Logger.logError(s"Attempting to find reference to ${expression} of classification ${x}")
    }
  }

}
