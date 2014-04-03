package joos.codegen.generators

import joos.ast.expressions.MethodInvocationExpression
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.assemgen._
import joos.assemgen.Register._

class MethodInvocationExpressionCodeGenerator(invocation: MethodInvocationExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment)
    extends AssemblyCodeGenerator {

  override def generate() {
    require(invocation.declaration != null)
    val method = invocation.declaration

    if (method.isNative) {
      // There should only be one parameter
      invocation.arguments.foreach(_.generate())
      appendText(
        :# ("[BEGIN] Native Method Call")
      )

      val registers = Register.values.filter(register => register != Eax && register != Esp)
      // Save all registers (except eax, esp)
      for (register <- registers) {
        appendText(push(register))
      }

      // Return value should be in eax
      appendText(call(method.uniqueName))

      // Restore all registers (except eax, esp)
      for (i <- registers.length - 1 to 0 by -1) {
        appendText(pop(registers(i)))
      }

      appendText(
        :# ("[END] Native Method Call")
      )

      return
    }

    invocation.expression.foreach(_.generate())
    invocation.arguments.foreach(_.generate())
  }
}
