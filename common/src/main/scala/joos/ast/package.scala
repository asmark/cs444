package joos

import joos.ast.declarations.SingleVariableDeclaration
import joos.ast.expressions._
import joos.ast.types.PrimitiveType._
import joos.ast.types._
import joos.syntax.tokens.TerminalToken
import joos.syntax.tokens.TokenKind

package object ast {
  implicit def toExpression(value: Int): IntegerLiteral = {
    IntegerLiteral(TerminalToken(value.toString, TokenKind.DecimalIntLiteral))
  }

  implicit def toQualifiedNameExpression(name: String): QualifiedNameExpression = {
    NameExpression(name).asInstanceOf[QualifiedNameExpression]
  }

  final lazy val StringCharArrayConstructor = findConstructor(StringType, IndexedSeq(ArrayType(CharType))).get

  final lazy val StringConcatMethod = findDeclaredMethod(StringType, "concat", IndexedSeq(StringType)).get

  private[this] def findConstructor(tipe: Type, parameterTypes: IndexedSeq[Type]) = {
    require(tipe.declaration != null)
    tipe.declaration.methods.find {
      method => method.isConstructor && matches(method.parameters, parameterTypes)
    }
  }

  private[this] def findDeclaredMethod(tipe: Type, name: String, parameterTypes: IndexedSeq[Type]) = {
    require(tipe.declaration != null)
    tipe.declaration.methods.find {
      method => method.name == SimpleNameExpression(name) && matches(method.parameters, parameterTypes)
    }
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
