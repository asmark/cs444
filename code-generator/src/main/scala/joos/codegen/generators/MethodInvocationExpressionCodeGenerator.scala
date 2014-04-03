package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.generators.commonlib._
import joos.ast.expressions.MethodInvocationExpression
import joos.codegen.AssemblyCodeGeneratorEnvironment

class MethodInvocationExpressionCodeGenerator(invocation: MethodInvocationExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment)
    extends AssemblyCodeGenerator {

  override def generate() {
    require(invocation.declaration != null)

    invocation.declaration.isNative match {
      case true => generateNativeMethodCall()
      case false => generateMethodCall()
    }
  }

  def generateMethodCall() {
    appendText(
      :#(s"[BEGIN] Method invocation expression ${invocation}")
    )

    invocation.arguments.foreach {
      argument =>
        appendText(
          :#("Evaluate parameter"),
          push(Ecx) :# "Save this",
          #>)

        argument.generate()

        appendText(
          #<,
          pop(Ecx) :# "Retrieve this",
          push(Eax) :# "Push parameter onto stack",
          emptyLine
        )
    }

    invocation.expression match {
      case Some(prefixType) => {
        appendText(
          :#(s"Evaluate method owner instance ${invocation.expression}"),
          #>
        )
        prefixType.generate()
        appendText(
          #<,
          :#(s"Placed method owner (${prefixType.expressionType}) into eax")
        )
      }
      case None => {
        appendText(
          mov(Eax, Ecx) :# s"Method owner is this in ${invocation}. Move this into eax",
          push(Eax) :# "Push prefix type argument for null check",
          call(nullCheck) :# "Call null check on prefix object",
          pop(Eax) :# "Pop prefix object back into eax"
        )
      }
    }


    val selectorIndex = environment.staticDataManager.getMethodIndex(invocation.declaration)

    appendText(
      mov(Eax, at(Eax)) :# "Move selector table into eax",
      mov(Eax, at(Eax + selectorIndex*4)) :# "Load method declaration into Eax",
      call(Eax) :# "Call method. Returns arguments in eax",
      add(Esp, 4 * invocation.arguments.size) :# "Pop arguments off stack",
      :#(s"[END] Method invocation expression ${invocation}"),
      emptyLine
    )
  }


  def generateNativeMethodCall() {
    val method = invocation.declaration
    if (method.isNative) {
      // There should only be one parameter
      assert(invocation.arguments.size == 1)
      invocation.arguments.foreach(_.generate())
      appendText(
        :#("[BEGIN] Native Method Call")
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
        :#("[END] Native Method Call")
      )

    }
  }

}
