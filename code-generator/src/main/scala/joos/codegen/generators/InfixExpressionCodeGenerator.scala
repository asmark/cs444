package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.Operator._
import joos.ast._
import joos.ast.expressions.InfixExpression
import joos.ast.types.NumericType
import joos.ast.types._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.codegen.generators.commonlib._
import joos.core.Logger

class InfixExpressionCodeGenerator(infix: InfixExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    val types = (infix.left.expressionType, infix.right.expressionType)
    infix.operator match {
      case Plus =>
        types match {
          case (StringType, StringType) => generateStringPlusString()
          case (StringType, right: PrimitiveType) =>
            Logger.logWarning(s"We don't support ${StringType} + ${right}")
          case (left: PrimitiveType, StringType) =>
            Logger.logWarning(s"We don't support ${left} + ${StringType}")
          case (StringType, NullType) =>
            Logger.logWarning("We don't support String + null")
          case (NullType, StringType) =>
            Logger.logWarning("We don't support null + String")
          case (StringType, right) =>
            Logger.logWarning(s"We don't support ${StringType} + ${right}")
          case (left, StringType) =>
            Logger.logWarning(s"We don't support ${left} + ${StringType}")
          case (_: NumericType, _: NumericType) => generateInfixOperation(addIntegers)
          case (left, right) =>
            Logger.logWarning(s"We don't support ${left} + ${right}")
        }
      case Minus => generateInfixOperation(subtractIntegers)
      case Multiply => generateInfixOperation(multiplyIntegers)
      case Divide => generateInfixOperation(divideIntegers)
      case Modulo => generateInfixOperation(moduloIntegers)
      case Less => generateInfixOperation(compareLess)
      case LessOrEqual => generateInfixOperation(compareLessEqual)
      case Greater => generateInfixOperation(compareGreater)
      case GreaterOrEqual => generateInfixOperation(compareGreaterEqual)
      case ConditionalAnd =>
        Logger.logWarning(s"We don't support ${ConditionalAnd}")
        generateInfixOperation(compareAnd)
      case BitwiseAnd => generateInfixOperation(compareAnd)
      case ConditionalOr =>
        Logger.logWarning(s"We don't support ${ConditionalOr}")
        generateInfixOperation(compareOr)
      case BitwiseInclusiveOr => generateInfixOperation(compareOr)
      case BitwiseExclusiveOr =>
        Logger.logWarning(s"We don't support ${BitwiseExclusiveOr}")
      case Equal => generateInfixOperation(compareEqual)
      case NotEqual => generateInfixOperation(compareNotEqual)
    }
  }

  private[this] def generateStringPlusString() {
//    // String + String
//    appendText(
//      :#("[BEGIN] String + String")
//    )
//    infix.left.generate()
//    appendText(push(Eax))
//    infix.right.generate()
//    appendText(
//      call(nullCheck),
//      pop(Ecx) :# "this = Left String",
//      push(Eax) :# "Push right string as the first parameter",
//      push(Ecx) :# "Push 'this'",
//      call(StringConcatMethod.uniqueName) :# "Call String.concat",
//      pop(Ecx) :# "Restore 'this'",
//      pop(Ebx),
//      :#("[END] String + String")
//    )
  }

  private[this] def generateInfixOperation(method: LabelReference) {

    appendText(:#(s"[BEGIN] Binary Operation ${infix.toString}"), emptyLine)

    appendText(
      push(Ecx) :# "Save this",
      :#("Evaluate left operand")
    )
    infix.left.generate()
    appendText(
      pop(Ecx) :# "Retrieve this",
      push(Eax) :# "Push left hand side as first parameter",
      emptyLine
    )

    appendText(
      push(Ecx) :# "Save this",
      :#("Evaluate right operand")
    )
    infix.right.generate()
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
}
