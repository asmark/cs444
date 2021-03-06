package joos.syntax.tokens

import joos.syntax.regexp._
import scala.language.postfixOps

object TokenKindRegexp {

  // Whitespace
  def Whitespace = {
    Alternation(" \n\r\t")
  }

  // Comments
  def EolComment = {
    Concatenation("//") + (allCharactersBut(NewlineChars) *) + newlineChar()
  }

  def TraditionalComment = {
    Concatenation("/*") +
      ((allCharactersBut(Seq('*')) | ((Atom('*') *) + allCharactersBut(Seq('*', '/')))) *) + (Atom('*') *) + Atom('/')
  }

  // Identifier
  // Regular Expression: [JavaLetter][JavaLetter or Digits]*
  def Id = {
    Alternation(ClassLetters) + (Alternation(ClassLetters + Digits) *)
  }

  // Keywords
  def Abstract = {
    Concatenation("abstract")
  }

  def Default = {
    Concatenation("default")
  }

  def If = {
    Concatenation("if")
  }

  def Private = {
    Concatenation("private")
  }

  def This = {
    Concatenation("this")
  }

  def Boolean = {
    Concatenation("boolean")
  }

  def Do = {
    Concatenation("do")
  }

  def Implements = {
    Concatenation("implements")
  }

  def Protected = {
    Concatenation("protected")
  }

  def Throw = {
    Concatenation("throw")
  }

  def Break = {
    Concatenation("break")
  }

  def Double = {
    Concatenation("double")
  }

  def Import = {
    Concatenation("import")
  }

  def Public = {
    Concatenation("public")
  }

  def Throws = {
    Concatenation("throws")
  }

  def Byte = {
    Concatenation("byte")
  }

  def Else = {
    Concatenation("else")
  }

  def InstanceOf = {
    Concatenation("instanceof")
  }

  def Return = {
    Concatenation("return")
  }

  def Transient = {
    Concatenation("transient")
  }

  def Case = {
    Concatenation("case")
  }

  def Extends = {
    Concatenation("extends")
  }

  def Int = {
    Concatenation("int")
  }

  def Short = {
    Concatenation("short")
  }

  def Try = {
    Concatenation("try")
  }

  def Catch = {
    Concatenation("catch")
  }

  def Final = {
    Concatenation("final")
  }

  def Interface = {
    Concatenation("interface")
  }

  def Static = {
    Concatenation("static")
  }

  def Void = {
    Concatenation("void")
  }

  def Char = {
    Concatenation("char")
  }

  def Finally = {
    Concatenation("finally")
  }

  def Long = {
    Concatenation("long")
  }

  def Strictfp = {
    Concatenation("strictfp")
  }

  def Volatile = {
    Concatenation("volatile")
  }

  def Class = {
    Concatenation("class")
  }

  def Float = {
    Concatenation("float")
  }

  def Native = {
    Concatenation("native")
  }

  def Super = {
    Concatenation("super")
  }

  def While = {
    Concatenation("while")
  }

  def Const = {
    Concatenation("const")
  }

  def For = {
    Concatenation("for")
  }

  def New = {
    Concatenation("new")
  }

  def Switch = {
    Concatenation("switch")
  }

  def Continue = {
    Concatenation("continue")
  }

  def Goto = {
    Concatenation("goto")
  }

  def Package = {
    Concatenation("package")
  }

  def Synchronized = {
    Concatenation("synchronized")
  }

  // Literals

  // IntegerLiteral

  // DecimalIntegerLiteral
  def DecimalIntLiteral = {
    Atom('0') | (Alternation(NonZeroDigits) + (Alternation(Digits) *))
  }

  def DecimalLongLiteral = {
    (Atom('0') | (Alternation(NonZeroDigits) + (Alternation(Digits) *))) + (Atom('l') | Atom('L'))
  }

  // HexNumeral
  // Regular Expression: (0x[0-9a-fA-F][0-9a-fA-F]*|0x[0-9a-fA-F][0-9a-fA-F]*(l|L))
  def HexIntLiteral = {
    val prefix = Atom('0') + (Atom('x') | Atom('X'))

    val non_postfix = prefix + Alternation(HexDigits) + (Alternation(HexDigits) *)
    val postfix =
      Concatenation(
        Seq(
          prefix,
          Alternation(HexDigits),
          Alternation(HexDigits) *,
          Atom('l') | Atom('L')
        )
      )

    non_postfix | postfix
  }

  // OctalIntegerLiteral
  // Regular Expression: 0[0-7][0-7]*
  def OctalIntLiteral = {
    Concatenation(Seq(Atom('0'), Alternation(OctalDigits), Alternation(OctalDigits) *)) + ~(Atom('l') | Atom('L'))
  }


  // FloatingPointLiteral
  // Regular Expression: TOO LONG
  def FloatingPointLiteral = {
    val first_form =
      Alternation(Digits) + (Alternation(Digits) *) +
        Atom('.') +
        ~(Alternation(Digits) + (Alternation(Digits) *)) +
        ~exponentPart() +
        ~floatTypeSuffix()

    val second_form =
      Atom('.') +
        Alternation(Digits) + (Alternation(Digits) *) +
        ~exponentPart() +
        ~floatTypeSuffix()

    val third_form =
      Alternation(Digits) + (Alternation(Digits) *) +
        exponentPart() +
        ~floatTypeSuffix()

    val fourth_form =
      Alternation(Digits) + (Alternation(Digits) *) +
        ~exponentPart() +
        floatTypeSuffix()

    first_form | second_form | third_form | fourth_form
  }

  // BooleanLiteral
  def True = {
    Concatenation("true")
  }

  def False = {
    Concatenation("false")
  }

  // CharacterLiteral
  // Regular Expression:
  def CharacterLiteral = {
    (Atom('\'') + singleCharacter() + Atom('\'')) | (Atom('\'') + escapeSequence() + Atom('\''))
  }

  // StringLiteral
  // Regular Expression: "StringCharacter*" | "", StringCharacter -> InputCharacter but not " or \ | EscapeSequence
  def StringLiteral = {
    Atom('\"') + (stringCharacter() *) + Atom('\"')
  }

  // Null Literal
  def NullLiteral = {
    Concatenation("null")
  }

  // Separators

  // (
  def LeftParen = {
    Atom('(')
  }

  // )
  def RightParen = {
    Atom(')')
  }

  // {
  def LeftBrace = {
    Atom('{')
  }

  // }
  def RightBrace = {
    Atom('}')
  }

  // [
  def LeftBracket = {
    Atom('[')
  }

  // ]
  def RightBracket = {
    Atom(']')
  }

  // ;
  def SemiColon = {
    Atom(';')
  }

  // ,
  def Comma = {
    Atom(',')
  }

  // .
  def Dot = {
    Atom('.')
  }

  // Operators
  def Assign = {
    Atom('=')
  }

  def Greater = {
    Atom('>')
  }

  def Less = {
    Atom('<')
  }

  def Exclamation = {
    Atom('!')
  }

  def Tilde = {
    Atom('~')
  }

  def Question = {
    Atom('?')
  }

  def Colon = {
    Atom(':')
  }

  def Equal = {
    Concatenation("==")
  }

  def LessEqual = {
    Concatenation("<=")
  }

  def GreaterEqual = {
    Concatenation(">=")
  }

  def NotEqual = {
    Concatenation("!=")
  }

  def And = {
    Concatenation("&&")
  }

  def Or = {
    Concatenation("||")
  }

  def Increment = {
    Concatenation("++")
  }

  def Decrement = {
    Concatenation("--")
  }

  def Plus = {
    Atom('+')
  }

  def Minus = {
    Atom('-')
  }

  def Multiply = {
    Atom('*')
  }

  def Divide = {
    Atom('/')
  }

  def BitAnd = {
    Atom('&')
  }

  def BitOr = {
    Atom('|')
  }

  def Carrot = {
    Atom('^')
  }

  def Modulo = {
    Atom('%')
  }

  def LeftShift = {
    Concatenation("<<")
  }

  def RightShift = {
    Concatenation(">>")
  }

  def UnsignedShift = {
    Concatenation(">>>")
  }

  def PlusAssign = {
    Concatenation("+=")
  }

  def MinusAssign = {
    Concatenation("-=")
  }

  def MultiplyAssign = {
    Concatenation("*=")
  }

  def DivideAssign = {
    Concatenation("/=")
  }

  def BitAndAssign = {
    Concatenation("&=")
  }

  def BitOrAssign = {
    Concatenation("|=")
  }

  def CarrotAssign = {
    Concatenation("^=")
  }

  def ModuloAssign = {
    Concatenation("%=")
  }

  def LeftShiftAssign = {
    Concatenation("<<=")
  }

  def RightShiftAssign = {
    Concatenation(">>=")
  }

  def UnsignedRightShiftAssign = {
    Concatenation(">>>=")
  }
}
