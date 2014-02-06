package joos

import joos.regexp._
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

package object tokens {

  final val Digits = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9').mkString("")
  final val NonZeroDigits = Digits.slice(1, Digits.length).mkString("")
  final val HexDigits = (Digits ++ Array(
    'a', 'b', 'c', 'd', 'e', 'f',
    'A', 'B', 'C', 'D', 'E', 'F'
  )).mkString("")
  final val OctalDigits = Digits.slice(0, 8).mkString("")

  final val Alphabets = {
    val lower = "abcdefghijklmnopqrstuvwxyz"
    val upper = lower.toUpperCase
    lower + upper
  }

  final val ClassLetters = {
    Alphabets + '_' + '$'
  }

  final val NewlineChars = {
    Array('\n', '\r')
  }

  def allCharactersBut(exclude: Seq[Char]) = {
    val validChars = allUnicode.filter(!exclude.contains(_))
    Alternation(validChars.mkString(""))
  }

  def newlineChar() = {
    Alternation(NewlineChars.mkString(""))
  }

  // helper functions for floating point
  def exponentPart(): RegularExpression = {
    (Atom('e') | Atom('E')) + ((~(Atom('+') | Atom('-'))) + Alternation(Digits) + (Alternation(Digits) *))
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
        Alternation(HexDigits),
        Alternation(HexDigits),
        Alternation(HexDigits),
        Alternation(HexDigits)
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
    (Atom('\\') + Alternation(OctalDigits)) |
      (Atom('\\') + Alternation(OctalDigits) + Alternation(OctalDigits)) |
      (Atom('\\') + zeroToThree() + Alternation(OctalDigits) + Alternation(OctalDigits))
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
