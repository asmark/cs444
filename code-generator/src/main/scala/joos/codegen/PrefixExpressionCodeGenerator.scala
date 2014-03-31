package joos.codegen

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.Operator._
import joos.ast.expressions.PrefixExpression
import joos.codegen.assembler._
import joos.core.Logger

class PrefixExpressionCodeGenerator(prefix: PrefixExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    prefix.operator match {
      case Plus =>
        Logger.logWarning("Invalid prefix operator: +")
      case Minus =>
        appendText(neg(Eax))
      case Not =>
        appendText(
          comment("[BEG] negation expr starts"),
          push(Ebx),
          xor(Ebx, Ebx),
          cmp(Eax, Ebx)
        )

        // if eax == 0 then eax = 1
        val randomLabel = nextLabel("negate_jmp")
        appendText(
          jne(LabelReference(randomLabel)),
          mov(Eax, 1)
        )

        // else eax = 0
        appendText(
          label(randomLabel),
          mov(Eax, 0),
          pop(Ebx),
          comment("[END] negation expr ends")
        )
    }
  }
}
