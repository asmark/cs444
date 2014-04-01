package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.Operator._
import joos.ast.expressions.PrefixExpression
import joos.codegen.AssemblyCodeGeneratorEnvironment

class PrefixExpressionCodeGenerator(prefix: PrefixExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {


  def integerNegation {
    appendText(
      #:(s"[BEGIN] Integer negation of ${prefix}"),
      emptyLine,
      #:("Evaluate expression"),
      #>
    )

    prefix.operand.generate()

    appendText(
      #<,
      neg(Eax) #: "Twos complement negation",
      #:("[END] Integer negation"),
      emptyLine
    )

  }

  def booleanNegation {
    appendText(
      #:(s"[BEGIN] Boolean negation of ${prefix}"),
      emptyLine,
      #:("Evaluate expression"),
      #>
    )

    prefix.operand.generate()

    appendText(
      #<,
      mov(Ebx, 1) #: "Move true into ebx",
      xor(Eax, Ebx) #: "XOR to negate boolean",
      #:("[END] Boolean negation"),
      emptyLine
    )
  }

  override def generate() {

    prefix.operator match {
      case Minus => integerNegation
      case Not => booleanNegation
    }
  }
}
