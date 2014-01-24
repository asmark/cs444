package joos.lexer

import scala.collection.mutable.ArrayBuffer

package object TokenKind {
  final val DIGITS = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9').mkString("")
  final val NON_ZERO_DIGITS = DIGITS.slice(1, DIGITS.length - 1).mkString("")
  final val HEX_DIGITS = (DIGITS ++ Array('a', 'b', 'c', 'd', 'e', 'f',
    'A', 'B', 'C', 'D', 'E', 'F')).mkString("")
  final val OCTAL_DIGITS = DIGITS.slice(0, 7).mkString("")

  final val ALPHABETS = {
    val lower = "abcdefghijklmnopqrstuvwxyz"
    val upper = lower.toUpperCase
    lower + upper
  }

  final val JAVA_LETTERS = {
    ALPHABETS + '_' + '$'
  }

  // helper functions for floating point
  def exponentPart(): RegularExpression = {
    (Atom('e') | Atom('E')) + Optional((Atom('+')|Atom('-'))) + Concatenation(DIGITS)
  }
  def floatTypeSuffix(): RegularExpression = {
    Alternation(Seq(Atom('f'), Atom('F'), Atom('d'), Atom('D')))
  }

  // Unicode
  def unicodeMarker(): RegularExpression = {
    Atom('u') + (Atom('u')*)
  }
  def unicodeEscape(): RegularExpression = {
    Concatenation(
      Seq(
        Atom(92.asInstanceOf[Char]),
        unicodeMarker(),
        Alternation(HEX_DIGITS),
        Alternation(HEX_DIGITS),
        Alternation(HEX_DIGITS),
        Alternation(HEX_DIGITS)
      )
    )
  }
  final val allUnicode: Array[Char] = {
    val ret = Array[Char](127)
    for (i <- ret.indices) {
      ret(i) = i.asInstanceOf[Char]
    }
    ret
  }
  def rawInputCharacter(): RegularExpression = {
    Alternation(allUnicode.mkString(""))
  }
  def unitcodeInputCharacter(): RegularExpression = {
    (rawInputCharacter() | unicodeEscape())
  }
  final val InputCharacter: Array[Char] = {
    val valid_unicode = ArrayBuffer(allUnicode : _*)
    valid_unicode -= 13.asInstanceOf[Char] // Remove CR
    valid_unicode -= 10.asInstanceOf[Char] // Remove LF
    valid_unicode.toArray
  }
  final val SingleCharacter: Array[Char] = {
    val valid_unicode = ArrayBuffer(InputCharacter : _*)
    valid_unicode -= 39.asInstanceOf[Char] // Remove CR
    valid_unicode -= 92.asInstanceOf[Char] // Remove LF
    valid_unicode.toArray
  }

  // Escape Sequence
  def zeroToThree(): RegularExpression = {
    Alternation(Seq(Atom('0'), Atom('1'), Atom('2'), Atom('3')))
  }
  def octalEscape(): RegularExpression = {
    Atom('\\') + Alternation(OCTAL_DIGITS) |
      Atom('\\') + Alternation(OCTAL_DIGITS) + Alternation(OCTAL_DIGITS) |
      Atom('\\') + zeroToThree() + Alternation(OCTAL_DIGITS) + Alternation(OCTAL_DIGITS)
  }
  def escapeSequence(): RegularExpression = {
    Alternation(
      Seq(
        Concatenation("\\b"),
        Concatenation("\\t"),
        Concatenation("\\n"),
        Concatenation("\\f"),
        Concatenation("\\r"),
        Concatenation("\\\""),
        Concatenation("\\\'"),
        Concatenation("\\\\"),
        octalEscape()
      )
    )
  }

  // String
  def stringCharacter(): RegularExpression = {
    val valid_unicode = ArrayBuffer(InputCharacter : _*)
    valid_unicode -= 34.asInstanceOf[Char] // Remove "
    valid_unicode -= 10.asInstanceOf[Char] // Remove LF
    valid_unicode.toArray

    Alternation(valid_unicode.mkString("")) | escapeSequence()
  }
}
