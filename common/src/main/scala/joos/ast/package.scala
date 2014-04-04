package joos

import joos.ast.declarations.SingleVariableDeclaration
import joos.ast.expressions.{NameExpression, QualifiedNameExpression, IntegerLiteral}
import joos.ast.types.PrimitiveType._
import joos.ast.types._
import joos.syntax.tokens.{TokenKind, TerminalToken}

package object ast {
  implicit def toExpression(value: Int): IntegerLiteral = {
    IntegerLiteral(TerminalToken(value.toString, TokenKind.DecimalIntLiteral))
  }

  implicit def toQualifiedNameExpression(name: String): QualifiedNameExpression = {
    NameExpression(name).asInstanceOf[QualifiedNameExpression]
  }

  final lazy val StringCharArrayConstructor = {
    require(StringType.declaration != null)
    val charType = IndexedSeq(ArrayType(CharType))
    StringType.declaration.methods.find {
      method =>
        method.isConstructor && matches(method.parameters, charType)
    }.get
  }

  private[this] def matches(as: IndexedSeq[SingleVariableDeclaration], bs: IndexedSeq[Type]): Boolean = {
    if (as.length != bs.length) {
      false
    } else {
      for (i <- 0 until as.length) {
        if (as(i).variableType != bs(i)) {
          return false
        }
      }
      true
    }
  }
}
