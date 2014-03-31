package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.Operator._
import joos.ast.expressions.PrefixExpression
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.core.Logger

class PrefixExpressionCodeGenerator(prefix: PrefixExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    prefix.operand.generate()

    prefix.operator match {
      case Plus =>
        Logger.logWarning("Invalid prefix operator: +")
      case Minus =>
        appendText(neg(Eax))
      case Not =>
        val endLabel = nextLabel("negate_end")
        val jumpLabel = nextLabel("negate_jmp")
        appendText(
          comment("[BEGIN] negation expr starts"),
          push(Ebx),
          xor(Ebx, Ebx),
          // if eax == 0 then eax = 1
          cmp(Eax, Ebx),
          jne(jumpLabel),
          mov(Eax, 1),
          jmp(endLabel),
          // else eax = 0
          label(jumpLabel),
          mov(Eax, 0),
          label(endLabel),
          pop(Ebx),
          comment("[END] negation expr ends")
        )
    }
  }
}
