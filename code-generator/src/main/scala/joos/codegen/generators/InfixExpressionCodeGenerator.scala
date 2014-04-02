package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.Operator._
import joos.ast.types._
import joos.codegen.generators.commonlib._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.expressions.InfixExpression
import joos.core.Logger

class InfixExpressionCodeGenerator(expression: InfixExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {

    (expression.left.expressionType, expression.right.expressionType) match {
      case (StringType, _) | (_, StringType) => {
        // TODO: Handle Strings
        Logger.logWarning("No support for string + operator yet.")
      }
      case _ => generateIntegerOperation
    }
  }

  def generateIntegerOperation {

    val method = expression.operator match {
      case Plus => addIntegers
      case Multiply => multiplyIntegers
      case Minus => subtractIntegers
      case Divide => divideIntegers
      case Modulo => moduloIntegers
      case ConditionalAnd | BitwiseAnd => compareAnd
      case ConditionalOr | BitwiseInclusiveOr => compareOr
      case Equal => compareEqual
      case NotEqual => compareNotEqual
      case Less => compareLess
      case LessOrEqual => compareLessEqual
      case Greater => compareGreater
      case GreaterOrEqual => compareGreaterEqual
      case op => {
        Logger.logWarning(s"${op} is not supported yet")
        return
      }
    }

    appendText(#:(s"[BEGIN] Integer Binary Operation ${expression.toString}"), emptyLine)

    appendText(#:("Evaluate left operand"))
    expression.left.generate()
    appendText(push(Eax) :# "Push Left hand side as first parameter", emptyLine)

    appendText(#:("Evaluate right operand"))
    expression.right.generate()
    appendText(push(Eax) :# "Push right hand side as second parameter", emptyLine)

    appendText(
      call(method),
      pop(Ebx) :# "Pop left operand",
      pop(Ebx) :# "Pop right operand",
      #:("[END] Integer Binary Operation")
    )
  }

}
