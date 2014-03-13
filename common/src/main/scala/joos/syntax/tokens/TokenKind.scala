package joos.syntax.tokens

import joos.core.Enumeration
import joos.syntax.regexp.RegularExpression

object TokenKind extends Enumeration {
  self =>

  type T = TokenKindValue
  type TokenKind = TokenKindValue

  class TokenKindValue(name: String, regexp: () => RegularExpression) extends Value(name) {
    self + this

    def getRegexp() = regexp() := this

    def getName() = name
  }


  def getHighestPriority(kinds: Set[TokenKind]): TokenKind = {
    values.find(kinds.contains).get
  }

  def kindToSymbol(kind: TokenKind) = {
    val tokenName = kind.asInstanceOf[TokenKindValue].getName()
    kindToSymbolMap.get(tokenName) match {
      case Some(symbol: String) => symbol
      case None => tokenName
    }
  }

  private[this] def TokenKindValue(name: String, regex: () => RegularExpression): TokenKindValue = {
    new TokenKindValue(name, regex)
  }

  // In order of priority

  // Keywords
  final val Abstract = TokenKindValue("Abstract", () => TokenKindRegexp.Abstract)

  final val Default = TokenKindValue("Default", () => TokenKindRegexp.Default)

  final val If = TokenKindValue("If", () => TokenKindRegexp.If)

  final val Private = TokenKindValue("Private", () => TokenKindRegexp.Private)

  final val This = TokenKindValue("This", () => TokenKindRegexp.This)

  final val Boolean = TokenKindValue("Boolean", () => TokenKindRegexp.Boolean)

  final val Do = TokenKindValue("Do", () => TokenKindRegexp.Do)

  final val Implements = TokenKindValue("Implements", () => TokenKindRegexp.Implements)

  final val Protected = TokenKindValue("Protected", () => TokenKindRegexp.Protected)

  final val Throw = TokenKindValue("Throw", () => TokenKindRegexp.Throw)

  final val Break = TokenKindValue("Break", () => TokenKindRegexp.Break)

  final val Double = TokenKindValue("Double", () => TokenKindRegexp.Double)

  final val Import = TokenKindValue("Import", () => TokenKindRegexp.Import)

  final val Public = TokenKindValue("Public", () => TokenKindRegexp.Public)

  final val Throws = TokenKindValue("Throws", () => TokenKindRegexp.Throws)

  final val Byte = TokenKindValue("Byte", () => TokenKindRegexp.Byte)

  final val Else = TokenKindValue("Else", () => TokenKindRegexp.Else)

  final val InstanceOf = TokenKindValue("InstanceOf", () => TokenKindRegexp.InstanceOf)

  final val Return = TokenKindValue("Return", () => TokenKindRegexp.Return)

  final val Transient = TokenKindValue("Transient", () => TokenKindRegexp.Transient)

  final val Case = TokenKindValue("Case", () => TokenKindRegexp.Case)

  final val Extends = TokenKindValue("Extends", () => TokenKindRegexp.Extends)

  final val Int = TokenKindValue("Int", () => TokenKindRegexp.Int)

  final val Short = TokenKindValue("Short", () => TokenKindRegexp.Short)

  final val Try = TokenKindValue("Try", () => TokenKindRegexp.Try)

  final val Catch = TokenKindValue("Catch", () => TokenKindRegexp.Catch)

  final val Final = TokenKindValue("Final", () => TokenKindRegexp.Final)

  final val Interface = TokenKindValue("Interface", () => TokenKindRegexp.Interface)

  final val Static = TokenKindValue("Static", () => TokenKindRegexp.Static)

  final val Void = TokenKindValue("Void", () => TokenKindRegexp.Void)

  final val Char = TokenKindValue("Char", () => TokenKindRegexp.Char)

  final val Finally = TokenKindValue("Finally", () => TokenKindRegexp.Finally)

  final val Long = TokenKindValue("Long", () => TokenKindRegexp.Long)

  final val Strictfp = TokenKindValue("Strictfp", () => TokenKindRegexp.Strictfp)

  final val Volatile = TokenKindValue("Volatile", () => TokenKindRegexp.Volatile)

  final val Class = TokenKindValue("Class", () => TokenKindRegexp.Class)

  final val Float = TokenKindValue("Float", () => TokenKindRegexp.Float)

  final val Native = TokenKindValue("Native", () => TokenKindRegexp.Native)

  final val Super = TokenKindValue("Super", () => TokenKindRegexp.Super)

  final val While = TokenKindValue("While", () => TokenKindRegexp.While)

  final val Const = TokenKindValue("Const", () => TokenKindRegexp.Const)

  final val For = TokenKindValue("For", () => TokenKindRegexp.For)

  final val New = TokenKindValue("New", () => TokenKindRegexp.New)

  final val Switch = TokenKindValue("Switch", () => TokenKindRegexp.Switch)

  final val Continue = TokenKindValue("Continue", () => TokenKindRegexp.Continue)

  final val Goto = TokenKindValue("Goto", () => TokenKindRegexp.Goto)

  final val Package = TokenKindValue("Package", () => TokenKindRegexp.Package)

  final val Synchronized = TokenKindValue("Synchronized", () => TokenKindRegexp.Synchronized)

  // Literals
  final val DecimalIntLiteral = TokenKindValue("DecimalIntLiteral", () => TokenKindRegexp.DecimalIntLiteral)

  final val DecimalLongLiteral = TokenKindValue("DecimalLongLiteral", () => TokenKindRegexp.DecimalLongLiteral)

  final val HexIntLiteral = TokenKindValue("HexIntLiteral", () => TokenKindRegexp.HexIntLiteral)

  final val OctalIntLiteral = TokenKindValue("OctalIntLiteral", () => TokenKindRegexp.OctalIntLiteral)

  final val FloatingPointLiteral = TokenKindValue("FloatingPointLiteral", () => TokenKindRegexp.FloatingPointLiteral)

  final val True = TokenKindValue("True", () => TokenKindRegexp.True)

  final val False = TokenKindValue("False", () => TokenKindRegexp.False)

  final val CharacterLiteral = TokenKindValue("CharacterLiteral", () => TokenKindRegexp.CharacterLiteral)

  final val StringLiteral = TokenKindValue("StringLiteral", () => TokenKindRegexp.StringLiteral)

  final val NullLiteral = TokenKindValue("NullLiteral", () => TokenKindRegexp.NullLiteral)

  // Separators
  final val LeftParen = TokenKindValue("LeftParen", () => TokenKindRegexp.LeftParen)

  final val RightParen = TokenKindValue("RightParen", () => TokenKindRegexp.RightParen)

  final val LeftBrace = TokenKindValue("LeftBrace", () => TokenKindRegexp.LeftBrace)

  final val RightBrace = TokenKindValue("RightBrace", () => TokenKindRegexp.RightBrace)

  final val LeftBracket = TokenKindValue("LeftBracket", () => TokenKindRegexp.LeftBracket)

  final val RightBracket = TokenKindValue("RightBracket", () => TokenKindRegexp.RightBracket)

  final val SemiColon = TokenKindValue("SemiColon", () => TokenKindRegexp.SemiColon)

  final val Comma = TokenKindValue("Comma", () => TokenKindRegexp.Comma)

  final val Dot = TokenKindValue("Dot", () => TokenKindRegexp.Dot)

  // Operators
  final val Assign = TokenKindValue("Assign", () => TokenKindRegexp.Assign)

  final val Greater = TokenKindValue("Greater", () => TokenKindRegexp.Greater)

  final val Less = TokenKindValue("Less", () => TokenKindRegexp.Less)

  final val Exclamation = TokenKindValue("Exclamation", () => TokenKindRegexp.Exclamation)

  final val Tilde = TokenKindValue("Tilde", () => TokenKindRegexp.Tilde)

  final val Question = TokenKindValue("Question", () => TokenKindRegexp.Question)

  final val Colon = TokenKindValue("Colon", () => TokenKindRegexp.Colon)

  final val Equal = TokenKindValue("Equal", () => TokenKindRegexp.Equal)

  final val LessEqual = TokenKindValue("LessEqual", () => TokenKindRegexp.LessEqual)

  final val GreaterEqual = TokenKindValue("GreaterEqual", () => TokenKindRegexp.GreaterEqual)

  final val NotEqual = TokenKindValue("NotEqual", () => TokenKindRegexp.NotEqual)

  final val And = TokenKindValue("And", () => TokenKindRegexp.And)

  final val Or = TokenKindValue("Or", () => TokenKindRegexp.Or)

  final val Increment = TokenKindValue("Increment", () => TokenKindRegexp.Increment)

  final val Decrement = TokenKindValue("Decrement", () => TokenKindRegexp.Decrement)

  final val Plus = TokenKindValue("Plus", () => TokenKindRegexp.Plus)

  final val Minus = TokenKindValue("Minus", () => TokenKindRegexp.Minus)

  final val Multiply = TokenKindValue("Multiply", () => TokenKindRegexp.Multiply)

  final val Divide = TokenKindValue("Divide", () => TokenKindRegexp.Divide)

  final val BitAnd = TokenKindValue("BitAnd", () => TokenKindRegexp.BitAnd)

  final val BitOr = TokenKindValue("BitOr", () => TokenKindRegexp.BitOr)

  final val Carrot = TokenKindValue("Carrot", () => TokenKindRegexp.Carrot)

  final val Modulo = TokenKindValue("Modulo", () => TokenKindRegexp.Modulo)

  final val LeftShift = TokenKindValue("LeftShift", () => TokenKindRegexp.LeftShift)

  final val RightShift = TokenKindValue("RightShift", () => TokenKindRegexp.RightShift)

  final val UnsignedShift = TokenKindValue("UnsignedShift", () => TokenKindRegexp.UnsignedShift)

  final val PlusAssign = TokenKindValue("PlusAssign", () => TokenKindRegexp.PlusAssign)

  final val MinusAssign = TokenKindValue("MinusAssign", () => TokenKindRegexp.MinusAssign)

  final val MultiplyAssign = TokenKindValue("MultiplyAssign", () => TokenKindRegexp.MultiplyAssign)

  final val DivideAssign = TokenKindValue("DivideAssign", () => TokenKindRegexp.DivideAssign)

  final val BitAndAssign = TokenKindValue("BitAndAssign", () => TokenKindRegexp.BitAndAssign)

  final val BitOrAssign = TokenKindValue("BitOrAssign", () => TokenKindRegexp.BitOrAssign)

  final val CarrotAssign = TokenKindValue("CarrotAssign", () => TokenKindRegexp.CarrotAssign)

  final val ModuloAssign = TokenKindValue("ModuloAssign", () => TokenKindRegexp.ModuloAssign)

  final val LeftShiftAssign = TokenKindValue("LeftShiftAssign", () => TokenKindRegexp.LeftShiftAssign)

  final val RightShiftAssign = TokenKindValue("RightShiftAssign", () => TokenKindRegexp.RightShiftAssign)

  final val UnsignedShiftRightAssign = TokenKindValue(
    "UnsignedRightShiftAssign",
    () => TokenKindRegexp.UnsignedRightShiftAssign
  )

  // Comments
  final val EolComment = TokenKindValue("EolComment", () => TokenKindRegexp.EolComment)

  final val TraditionalComment = TokenKindValue("TraditionalComment", () => TokenKindRegexp.TraditionalComment)

  // Identifier
  final val Id = TokenKindValue("Id", () => TokenKindRegexp.Id)

  // Whitespace
  final val Whitespace = TokenKindValue("Whitespace", () => TokenKindRegexp.Whitespace)


  final val kindToSymbolMap = Map(
    "Id" -> "Identifier",

    "Abstract" -> "abstract",
    "Default" -> "default",
    "If" -> "if",
    "Private" -> "private",
    "This" -> "this",
    "Boolean" -> "boolean",
    "Do" -> "do",
    "Implements" -> "implements",
    "Protected" -> "protected",
    "Throw" -> "throw",
    "Break" -> "break",
    "Double" -> "double",
    "Import" -> "import",
    "Public" -> "public",
    "Throws" -> "throws",
    "Byte" -> "byte",
    "Else" -> "else",
    "InstanceOf" -> "instanceof",
    "Return" -> "return",
    "Transient" -> "transient",
    "Case" -> "case",
    "Extends" -> "extends",
    "Int" -> "int",
    "Short" -> "short",
    "Try" -> "try",
    "Catch" -> "catch",
    "Final" -> "final",
    "Interface" -> "interface",
    "Static" -> "static",
    "Void" -> "void",
    "Char" -> "char",
    "Finally" -> "finally",
    "Long" -> "long",
    "Strictfp" -> "strictfp",
    "Volatile" -> "volatile",
    "Class" -> "class",
    "Float" -> "float",
    "Native" -> "native",
    "Super" -> "super",
    "While" -> "while",
    "Const" -> "const",
    "For" -> "for",
    "New" -> "new",
    "Switch" -> "switch",
    "Continue" -> "continue",
    "Goto" -> "goto",
    "Package" -> "package",
    "Synchronized" -> "synchronized",

    "LeftParen" -> "(",
    "RightParen" -> ")",
    "LeftBrace" -> "{",
    "RightBrace" -> "}",
    "LeftBracket" -> "[",
    "RightBracket" -> "]",
    "SemiColon" -> ";",
    "Comma" -> ",",
    "Dot" -> ".",

    "Assign" -> "=",
    "Greater" -> ">",
    "Less" -> "<",
    "Exclamation" -> "!",
    "Tilde" -> "~",
    "Question" -> "?",
    "Colon" -> ":",
    "Equal" -> "==",
    "LessEqual" -> "<=",
    "GreaterEqual" -> ">=",
    "NotEqual" -> "!=",
    "And" -> "&&",
    "Or" -> "||",
    "Increment" -> "++",
    "Decrement" -> "--",
    "Plus" -> "+",
    "Minus" -> "-",
    "Multiply" -> "*",
    "Divide" -> "/",
    "BitAnd" -> "&",
    "BitOr" -> "|",
    "Carrot" -> "^",
    "Modulo" -> "%",
    "LeftShift" -> "<<",
    "RightShift" -> ">>",
    "UnsignedShift" -> ">>>",
    "PlusAssign" -> "+=",
    "MinusAssign" -> "-=",
    "MultiplyAssign" -> "*=",
    "DivideAssign" -> "/=",
    "BitAndAssign" -> "&=",
    "BitOrAssign" -> "|=",
    "CarrotAssign" -> "^=",
    "ModuloAssign" -> "%=",
    "LeftShiftAssign" -> "<<=",
    "RightShiftAssign" -> ">>=",
    "UnsignedRightShiftAssign" -> ">>>="
  )

}
