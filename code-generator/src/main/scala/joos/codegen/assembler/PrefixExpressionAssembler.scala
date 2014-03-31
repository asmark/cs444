package joos.codegen.assembler

import joos.ast.expressions.PrefixExpression
import joos.codegen.AssemblyFileManager
import joos.ast.Operator._
import joos.assemgen._
import joos.assemgen.Register._
import joos.core.Logger

class PrefixExpressionAssembler(expression: PrefixExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {
    expression.operator match {
      case Plus => {
        Logger.logWarning("Invalid prefix operator: +")
      }
      case Minus => {
        assemblyManager.appendText(neg(Eax))
      }
      case Not => {
        assemblyManager.appendText(comment("[BEG] negation expr starts"))
        assemblyManager.appendText(push(Ebx))
        assemblyManager.appendText(xor(Ebx, Ebx))
        assemblyManager.appendText(cmp(Eax, Ebx))

        // if eax == 0 then eax = 1
        val randomLabel = nextLabel("negate_jmp")
        assemblyManager.appendText(jne(LabelReference(randomLabel)))
        assemblyManager.appendText(mov(Eax, 1))

        // else eax = 0
        assemblyManager.appendText(label(randomLabel))
        assemblyManager.appendText(mov(Eax, 0))

        assemblyManager.appendText(pop(Ebx))
        assemblyManager.appendText(comment("[END] negation expr ends"))
      }
    }
  }
}
