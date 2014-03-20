package joos.analysis.reachability

import joos.analysis.reachability.ExpressionEvaluator._
import joos.ast.ExpressionDispatcher
import joos.ast.Operator._
import joos.ast.expressions.BooleanLiteral._
import joos.ast.expressions._
import scala.Some

class ExpressionEvaluator extends ExpressionDispatcher {
  private var value: Option[Any] = None

  override def apply(literal: NullLiteral) {
    value = Some(null)
  }

  override def apply(literal: StringLiteral) {
    value = Some(literal.text)
  }

  override def apply(literal: CharacterLiteral) {
    value = Some(literal.token.lexeme(1))
  }

  override def apply(literal: IntegerLiteral) {
    value = Some(literal.token.lexeme.toLong)
  }

  override def apply(literal: BooleanLiteral) {
    value = literal match {
      case TrueLiteral => Some(true)
      case FalseLiteral => Some(false)
    }
  }

  override def apply(parenthesis: ParenthesizedExpression) {
    value = evaluate(parenthesis.expression)
  }

  override def apply(prefix: PrefixExpression) {
    value = evaluate(prefix.operand) match {
      case Some(value: Boolean) => Some(!value)
      case _ => None
    }
  }

  override def apply(infix: InfixExpression) {
    val left = evaluate(infix.left)
    val right = evaluate(infix.right)

    if (left.isEmpty || right.isEmpty) {
      value = None
      return
    }

    val values = (left.get, right.get)
    value = infix.operator match {
      case Plus =>
        values match {
          case (left: String, right) => Some(left + right)
          case (left, right: String) => Some(left + right)
          case (left: Long, right: Long) => Some(left + right)
        }
      case Minus =>
        values match {
          case (left: Long, right: Long) => Some(left - right)
        }
      case Multiply =>
        values match {
          case (left: Long, right: Long) => Some(left * right)
        }
      case Divide =>
        values match {
          case (left: Long, right: Long) =>
            if (right != 0) Some(left / right) else None
        }
      case Modulo =>
        values match {
          case (left: Long, right: Long) => Some(left % right)
        }
      case Less =>
        values match {
          case (left: Long, right: Long) => Some(left < right)
        }
      case LessOrEqual =>
        values match {
          case (left: Long, right: Long) => Some(left <= right)
        }
      case Greater =>
        values match {
          case (left: Long, right: Long) => Some(left > right)
        }
      case GreaterOrEqual =>
        values match {
          case (left: Long, right: Long) => Some(left >= right)
        }
      case Equal =>
        values match {
          case (left: Boolean, right: Boolean) => Some(left == right)
          case (null, null) => Some(true)
          case (left: Long, right: Long) => Some(left == right)
          case _ => None
        }
      case ConditionalAnd | BitwiseAnd =>
        values match {
          case (left: Boolean, right: Boolean) => Some(left && right)
        }
      case ConditionalOr | BitwiseInclusiveOr =>
        values match {
          case (left: Boolean, right: Boolean) => Some(left || right)
        }
      case BitwiseExclusiveOr =>
        values match {
          case (left: Boolean, right: Boolean) => Some(left ^ right)
        }
    }
  }
}

object ExpressionEvaluator {
  def evaluate(expression: Expression): Option[Any] = {
    val evaluator = new ExpressionEvaluator
    evaluator(expression)
    evaluator.value
  }
}
