package joos.codegen.generators

import joos.ast.expressions.SimpleNameExpression
import joos.codegen.AssemblyFileManager
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.core.Logger

class SimpleNameExpressionCodeGenerator(expression: SimpleNameExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    try {
      val slot = environment.getVariableSlot(expression)
      appendText(
        mov(Edx, Ebp),
        sub(Edx, slot*4) :# s"Retrieve lvalue address of variable ${expression.standardName}",
        mov(Eax, at(Edx)) :# s"Retrieve variable ${expression.standardName}"
      )
    } catch {
      // TODO: Implicit this for fields reference
      case e: Exception => Logger.logWarning(s"Got an exception when trying to find variable ${expression.standardName}. This is probably a instance field.")
    }

  }

}
