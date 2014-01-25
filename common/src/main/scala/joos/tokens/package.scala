package joos

import joos.regexp._
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

package object tokens {

  final val DIGITS = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9').mkString("")
  final val NON_ZERO_DIGITS = DIGITS.slice(1, DIGITS.length - 1).mkString("")
  final val HEX_DIGITS = (DIGITS ++ Array('a', 'b', 'c', 'd', 'e', 'f',
    'A', 'B', 'C', 'D', 'E', 'F')).mkString("")
  final val OCTAL_DIGITS = DIGITS.slice(0, 8).mkString("")

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
    (Atom('e') | Atom('E')) + ((~(Atom('+') | Atom('-'))) + Alternation(DIGITS) + (Alternation(DIGITS) *))
  }

  def floatTypeSuffix(): RegularExpression = {
    Alternation(Seq(Atom('f'), Atom('F'), Atom('d'), Atom('D')))
  }

  // Unicode
  def unicodeMarker(): RegularExpression = {
    Atom('u') + (Atom('u') *)
  }

  def unicodeEscape(): RegularExpression = {
    Concatenation(
      Seq(
        Atom('\\'),
        unicodeMarker(),
        Alternation(HEX_DIGITS),
        Alternation(HEX_DIGITS),
        Alternation(HEX_DIGITS),
        Alternation(HEX_DIGITS)
      )
    )
  }

  final val allUnicode: Array[Char] = {
    val ret = new Array[Char](127)
    for (i <- ret.indices) {
      ret(i) = i.asInstanceOf[Char]
    }
    ret
  }

  def rawInputCharacter(): RegularExpression = {
    Alternation(allUnicode.mkString(""))
  }

  def unitcodeInputCharacter(): RegularExpression = {
    rawInputCharacter() | unicodeEscape()
  }

  def inputCharacter(): RegularExpression = {
    val raw_input_char = ArrayBuffer(allUnicode: _*)
    raw_input_char -= 13.asInstanceOf[Char] // Remove CR
    raw_input_char -= 10.asInstanceOf[Char] // Remove LF
    Alternation(raw_input_char.mkString("")) | unicodeEscape()
  }

  def singleCharacter(): RegularExpression = {
    val raw_input_char = ArrayBuffer(allUnicode: _*)
    raw_input_char -= 13.asInstanceOf[Char] // Remove CR
    raw_input_char -= 10.asInstanceOf[Char] // Remove LF
    raw_input_char -= 39.asInstanceOf[Char] // Remove "
    raw_input_char -= 92.asInstanceOf[Char] // Remove \
    Alternation(raw_input_char.mkString("")) | unicodeEscape()
  }

  // Escape Sequence
  def zeroToThree(): RegularExpression = {
    Alternation(Seq(Atom('0'), Atom('1'), Atom('2'), Atom('3')))
  }

  def octalEscape(): RegularExpression = {
    (Atom('\\') + Alternation(OCTAL_DIGITS)) |
      (Atom('\\') + Alternation(OCTAL_DIGITS) + Alternation(OCTAL_DIGITS)) |
      (Atom('\\') + zeroToThree() + Alternation(OCTAL_DIGITS) + Alternation(OCTAL_DIGITS))
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
    val raw_input_char = ArrayBuffer(allUnicode: _*)
    raw_input_char -= 13.asInstanceOf[Char] // Remove CR
    raw_input_char -= 10.asInstanceOf[Char] // Remove LF
    raw_input_char -= 34.asInstanceOf[Char] // Remove "
    raw_input_char -= 10.asInstanceOf[Char] // Remove LF
    Alternation(raw_input_char.mkString("")) | escapeSequence()
  }
}
