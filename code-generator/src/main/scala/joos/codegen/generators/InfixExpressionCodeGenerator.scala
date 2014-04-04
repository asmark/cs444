package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.Operator._
import joos.ast.expressions.InfixExpression
import joos.ast.types._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.codegen.generators.commonlib._
import joos.core.Logger
import joos.ast._

class InfixExpressionCodeGenerator(expression: InfixExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {

    (expression.left.expressionType, expression.right.expressionType) match {
      case (StringType, _) | (_, StringType) =>
        generateStringOperation
      case (SimpleType(_) | ArrayType(_,_), SimpleType(_) | ArrayType(_,_)) => generateObjectOperation
      case _ => generateIntegerOperation
    }
  }

  private def generateInfixOperation(method: LabelReference) {

    appendText(:#(s"[BEGIN] Binary Operation ${expression.toString}"), emptyLine)

    appendText(
      push(Ecx) :# "Save this",
      :#("Evaluate left operand")
    )
    expression.left.generate()
    appendText(
      pop(Ecx) :# "Retrieve this",
      push(Eax) :# "Push left hand side as first parameter",
      emptyLine
    )

    appendText(
      push(Ecx) :# "Save this",
      :#("Evaluate right operand")
    )
    expression.right.generate()
    appendText(
      pop(Ecx) :# "Retrieve this",
      push(Eax) :# "Push right hand side as first parameter",
      emptyLine
    )

    // TODO: Maybe have to save "this" before going into method?
    appendText(
      call(method),
      pop(Ebx) :# "Pop left operand",
      pop(Ebx) :# "Pop right operand",
      :#("[END] Binary Operation")
    )
  }


  private def generateIntegerOperation {

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
    generateInfixOperation(method)
  }

  private def generateStringOperation {
    if (expression.operator == Plus) {
      // String + String
      appendText(
        :# ("[BEGIN] String + String")
      )
      expression.left.generate()
      appendText(push(Eax))
      expression.right.generate()
      appendText(
        pop(Ecx) :# "this = Left String",
        push(Eax) :# "Push right string as the first parameter",
        push(Ecx) :# "Push 'this'",
        call(StringConcatMethod.uniqueName) :# "Call String.concat",
        pop(Ecx) :# "Restore 'this'",
        pop(Ebx),
        :# ("[END] String + String")
      )
      return
    }

    val method = expression.operator match {
      case Equal => compareEqual
      case NotEqual => compareNotEqual
    }

    generateInfixOperation(method)
  }

  private def generateObjectOperation {
    val method = expression.operator match {
      case Equal => compareEqual
      case NotEqual => compareNotEqual
    }

    generateInfixOperation(method)
  }

}
