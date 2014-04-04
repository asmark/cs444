package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.InstanceOfExpression
import joos.ast.types._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.core.Logger

class InstanceOfExpressionCodeGenerator(expression: InstanceOfExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    expression.expression.expressionType match {
      case leftType: SimpleType => {
        require(leftType.declaration != null)
        val rightType = expression.classType
        val subtypeIndex = environment.staticDataManager.getTypeIndex(rightType.declaration)
        appendText(
          :#(s"[BEGIN] InstanceOf check ${expression}"),
          :#("Look up left hand side"),
          #>
        )
        expression.expression.generate()
        appendText(
          #<,
          emptyLine,
          mov(Eax, at(Eax + SubtypeTableOffset)) :# "Put address of subtype table in eax",
          mov(Eax, at(Eax + 4 * subtypeIndex)) :# "Look up value in subtype table for instance check",
          :#(s"[END] InstanceOf check ${expression}")
        )
      }

        // TODO: Array instanceof check
      case _ =>
        Logger.logWarning(s"No Support for ${expression.expressionType} in instanceof checks in ${expression}")
    }
  }

}
