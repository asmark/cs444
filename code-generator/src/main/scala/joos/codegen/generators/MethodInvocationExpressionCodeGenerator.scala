package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.MethodInvocationExpression
import joos.codegen.AssemblyCodeGeneratorEnvironment
import scala.Some

class MethodInvocationExpressionCodeGenerator(invocation: MethodInvocationExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment)
    extends AssemblyCodeGenerator {

  override def generate() {


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
          mov(Eax, Ecx) :# s"Method owner is this in ${invocation.expression}. Move this into eax"
        )
      }
    }

  }

  val selectorIndex = environment.staticDataManager.getMethodIndex(invocation.methodName.declaration)

  appendText(
    mov(Eax, at(Eax)) :# "Move selector table into eax",
    mov(Eax, at(Eax + selectorIndex)) :# "Load method declaration into Eax",
    call(Eax) :# "Call method. Returns arguments in eax",
    add(Esp, 4 * invocation.arguments.size) :# "Pop arguments off stack",
    :#(s"[END] Method invocation expression ${invocation}"),
    emptyLine
  )

}
