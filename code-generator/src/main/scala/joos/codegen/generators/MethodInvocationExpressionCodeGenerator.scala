package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.{Expression, SimpleNameExpression, QualifiedNameExpression, MethodInvocationExpression}
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.codegen.generators.commonlib._

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

  private def generateMethodCall() {
    appendText(
      :#(s"[BEGIN] Method invocation expression ${invocation}")
    )

    invocation.expression match {
      case Some(prefixType) => {
        getPrefixType(prefixType)
      }

      case None => {
        invocation.methodName match {
          case QualifiedNameExpression(prefixType, methodName) => {
            getPrefixType(prefixType)
          }
          case SimpleNameExpression(methodName) => {
            appendText(
              mov(Eax, Ecx) :# s"Method owner is this in ${invocation}. Move this into eax"
            )
          }
        }
        checkNullInstance()
      }
    }

    appendText(mov(Ebx, Eax) :# "Move method owner into Ebx")

    invocation.arguments.foreach {
      argument =>
        appendText(
          :#("Evaluate parameter"),
          push(Ecx) :# "Save this",
          push(Ebx) :# "Save method owner",
          #>)

        argument.generate()

        appendText(
          #<,
          pop(Ebx) :# "Retrieve method owner",
          pop(Ecx) :# "Retrieve this",
          push(Eax) :# "Push parameter onto stack",
          emptyLine
        )
    }

    appendText(mov(Eax, Ebx) :# "Move method owner into Eax")

    val selectorIndex = environment.staticDataManager.getMethodIndex(invocation.declaration)
    appendText(
      mov(Eax, at(Eax)) :# "Move selector table into eax",
      mov(Eax, at(Eax + selectorIndex * 4)) :# "Load method declaration into Eax",
      call(Eax) :# "Call method. Returns arguments in eax",
      add(Esp, 4 * invocation.arguments.size) :# "Pop arguments off stack",
      :#(s"[END] Method invocation expression ${invocation}"),
      emptyLine
    )
  }

  private def getPrefixType(prefix: Expression) {
    appendText(
      :#(s"Evaluate method owner instance ${prefix}"),
      #>
    )
    prefix.generate()
    appendText(
      #<,
      :#(s"Placed method owner (${prefix}) into eax")
    )
  }


  private def generateNativeMethodCall() {
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

  private def checkNullInstance() {
    appendText(
      push(Eax) :# "Push prefix type argument for null check",
      call(nullCheck) :# "Call null check on prefix object",
      pop(Eax) :# "Pop prefix object back into eax"
    )
  }

}
