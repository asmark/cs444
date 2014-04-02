package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.NameClassification._
import joos.ast.expressions.QualifiedNameExpression
import joos.ast.types.{ArrayType, SimpleType}
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.core.Logger

class QualifiedNameExpressionCodeGenerator(expression: QualifiedNameExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {

    expression.nameClassification match {

      case InstanceFieldName => {
        expression.qualifier.generate()
        val declaration = expression.qualifier.expressionType match {
          case simple: SimpleType => simple.declaration
          case array: ArrayType =>
            Logger.logWarning(s"No support for array accesses yet in ${expression}")
            return
        }

        val slot = declaration.getFieldSlot(expression.name)
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

      case e =>
        Logger.logWarning(s"No support for accessing ${expression} of classification ${e}")
    }

  }

}
