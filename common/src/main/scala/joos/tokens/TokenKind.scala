package joos.tokens

import joos.regexp.RegularExpression

object TokenKind extends Enumeration {
  type TokenKind = Value

  case class TokenKindValue(val name: String, regexp: Function0[RegularExpression]) extends Val(name) {
    def getRegexp() = regexp() := this
  }

  def getHighestPriority(kinds: Set[TokenKind]): TokenKind = {
    return values.find(kind => kinds.contains(kind)).get
  }

  // In order of priority

  // Keywords
  final val Abstract = TokenKindValue("Abstract", () => TokenKindRegexp.Abstract )

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

  final val Import = TokenKindValue("Import", () => TokenKindRegexp.Public)

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
  final val DecimalInteger = TokenKindValue("DecimalInteger", () => TokenKindRegexp.DecimalIntLiteral)

  final val HexInteger = TokenKindValue("HexInteger", () => TokenKindRegexp.HexIntLiteral)

  final val OctalInteger = TokenKindValue("OctalInteger", () => TokenKindRegexp.OctalIntLiteral)

  final val FloatingPoint = TokenKindValue("FloatingPoint", () => TokenKindRegexp.FloatLiteral)

  final val True = TokenKindValue("True", () => TokenKindRegexp.True)

  final val False = TokenKindValue("False", () => TokenKindRegexp.False)

  final val Character = TokenKindValue("Character", () => TokenKindRegexp.CharacterLiteral)

  final val String = TokenKindValue("String", () => TokenKindRegexp.StringLiteral)

  final val Null = TokenKindValue("Null", () => TokenKindRegexp.Null)

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
  final val Assign = TokenKindValue("=", () => TokenKindRegexp.Assign)

  final val Greater = TokenKindValue("Greater", () => TokenKindRegexp.Greater)

  final val Less = TokenKindValue("Less", () => TokenKindRegexp.Less)

  final val Exclamation = TokenKindValue("Exclamation", () => TokenKindRegexp.Exclamation)

  final val Grave = TokenKindValue("Grave", () => TokenKindRegexp.Grave)

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

  final val DivideAssign = TokenKindValue("Minus", () => TokenKindRegexp.Minus)

  final val BitAndAssign = TokenKindValue("BitAndAssign", () => TokenKindRegexp.BitAndAssign)

  final val BitOrAssign = TokenKindValue("BitOrAssign", () => TokenKindRegexp.BitOrAssign)

  final val CarrotAssign = TokenKindValue("CarrotAssign", () => TokenKindRegexp.CarrotAssign)

  final val ModuloAssign = TokenKindValue("ModuloAssign", () => TokenKindRegexp.ModuloAssign)

  final val LeftShiftAssign = TokenKindValue("LeftShiftAssign", () => TokenKindRegexp.LeftShiftAssign)

  final val RightShiftAssign = TokenKindValue("RightShiftAssign", () => TokenKindRegexp.RightShiftAssign)

  final val UsignedRightShiftAssign = TokenKindValue("UsignedRightShiftAssign", () => TokenKindRegexp.UsignedRightShiftAssign)

  // Comments
  final val TraditionalCommentPrefix =
    TokenKindValue("TraditionalCommentPrefix", () => TokenKindRegexp.TraditionalCommentPrefix)

  final val TraditionalCommentPostfix =
    TokenKindValue("TraditionalCommentPostfix", () => TokenKindRegexp.TraditionalCommentPostfix)

  final val EolCommentPrefix = TokenKindValue("EolCommentPrefix", () => TokenKindRegexp.EolCommentPrefix)

  // Identifier
  final val Id = TokenKindValue("Id", () => TokenKindRegexp.Id)

}
