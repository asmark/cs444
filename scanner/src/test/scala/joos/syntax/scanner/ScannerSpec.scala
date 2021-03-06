package joos.syntax.scanner

import java.io.FileInputStream
import joos.resources
import joos.syntax.automata.{Dfa, AcceptingDfaNode, NonAcceptingDfaNode}
import joos.syntax.regexp.{Concatenation, Alternation}
import joos.syntax.tokens.TokenKind.TokenKindValue
import joos.syntax.tokens.{TerminalToken, TokenKind}
import org.scalatest.{FlatSpec, Matchers}

class ScannerSpec extends FlatSpec with Matchers {

  private val CharacterA = 'A'
  private val CharacterB = 'B'
  private val CharacterC = 'C'

  private val TokenKind1 = TokenKind.Id
  private val TokenKind2 = TokenKind.Dot

  private val testDfaNoLoops = NonAcceptingDfaNode().
    addTransition(CharacterA, AcceptingDfaNode(TokenKind1)).
    addTransition(
      CharacterB,
      AcceptingDfaNode(TokenKind2).addTransition(CharacterA, NonAcceptingDfaNode())
    )

  private val testDfaWithLoops = NonAcceptingDfaNode()
  testDfaWithLoops.addTransition(CharacterA, testDfaWithLoops).
    addTransition(
      CharacterB,
      AcceptingDfaNode(TokenKind2).addTransition(CharacterA, AcceptingDfaNode(TokenKind1))
    )

  private val testDfaDeadEnds = NonAcceptingDfaNode().
    addTransition(
      CharacterA,
      NonAcceptingDfaNode().addTransition(
        CharacterC,
        AcceptingDfaNode(TokenKind1)
      )
    ).
    addTransition(
      CharacterB,
      NonAcceptingDfaNode().addTransition(
        CharacterB,
        AcceptingDfaNode(TokenKind2).addTransition(
          CharacterA,
          NonAcceptingDfaNode().addTransition(
            CharacterC,
            NonAcceptingDfaNode()
          )
        )
      )
    )

  lazy val joosDfa = Dfa.deserialize(new FileInputStream(resources.lexerDfa))

  // Tests begin here
  "A state with no transition" should "backtrack once to accepting nodes" in {
    val scanner = new Scanner(Dfa(testDfaDeadEnds))
    val input = Seq(CharacterB, CharacterB, CharacterA, CharacterC, CharacterB, CharacterB)

    input.foreach(char => scanner.scan(char))

    val tokens = scanner.generateTokens()
    tokens.map(token => token.kind) should contain theSameElementsInOrderAs Seq(TokenKind2, TokenKind1, TokenKind2)
  }

  it should "backtrack twice to accepting nodes" in {
    val scanner = new Scanner(Dfa(testDfaNoLoops))
    val input = Seq(CharacterB, CharacterA, CharacterB)

    input.foreach(char => scanner.scan(char))

    val tokens = scanner.generateTokens()
    tokens.map(token => token.kind) should contain theSameElementsInOrderAs Seq(TokenKind2, TokenKind1, TokenKind2)
  }

  "A state with loops" should "loop correctly" in {
    val scanner = new Scanner(Dfa(testDfaWithLoops))
    val input = Seq(CharacterA, CharacterA, CharacterA, CharacterB, CharacterA, CharacterA, CharacterB)

    input.foreach(char => scanner.scan(char))

    val tokens = scanner.generateTokens()
    tokens.map(token => token.kind) should contain theSameElementsInOrderAs Seq(TokenKind1, TokenKind2)
  }

  "Non-tokenizable input" should "throw a scanning exception" in {
    val scanner = new Scanner(Dfa(testDfaWithLoops))
    val input = Seq(CharacterA, CharacterA, CharacterA, CharacterB, CharacterB, CharacterB, CharacterA, CharacterA)

    intercept[ScanningException] {
      input.foreach(char => scanner.scan(char))
      scanner.generateTokens()
    }
  }

  "An epsilon closure with multiple accepting states" should "accept the highest priority token" in {
    val testRegexp = Concatenation("final") := TokenKind.Id := TokenKind.Final

    val scanner = Scanner(Dfa(testRegexp))

    "final".toCharArray.foreach(c => scanner.scan(c))
    val tokens = scanner.generateTokens()
    tokens should have length 1
    tokens should contain(TerminalToken("final", TokenKind.Final))
  }

  behavior of "A static word regular expression (final) to DFA conversion"

  it should "accept tokenizable (final) inputs" in {
    val scanner = Scanner(Dfa(TokenKind.Final.getRegexp()))

    "final".toCharArray.foreach(c => scanner.scan(c))
    val tokens = scanner.generateTokens()
    tokens should have length 1
    tokens should contain(TerminalToken("final", TokenKind.Final))
  }

  it should "reject non-tokenizable (final3) inputs" in {
    val scanner = Scanner(Dfa(TokenKind.Final.getRegexp()))

    intercept[ScanningException] {
      "final3".toCharArray.foreach(c => scanner.scan(c))
      scanner.generateTokens()
    }
  }

  behavior of "An alternating word regular expression (T|test) to DFA conversion"

  it should "accept tokenizable (test) inputs" in {
    val testRegexp = Alternation("tT") + Concatenation("est") := TokenKind1

    val scanner = Scanner(Dfa(testRegexp))

    "test".toCharArray.foreach(c => scanner.scan(c))

    val tokens = scanner.generateTokens()
    tokens should have length 1
    tokens should contain(TerminalToken("test", TokenKind1))
  }

  it should "accept tokenizable (Test) inputs" in {
    val testRegexp = Alternation("tT") + Concatenation("est") := TokenKind1
    val scanner = Scanner(Dfa(testRegexp))

    "Test".toCharArray.foreach(c => scanner.scan(c))

    val tokens = scanner.generateTokens()
    tokens should have length 1
    tokens should contain(TerminalToken("Test", TokenKind1))
  }


  behavior of "A looping word regular expression (ID) to DFA conversion"

  it should "accept tokenizable (t998) inputs" in {
    val scanner = Scanner(Dfa(TokenKind.Id.getRegexp()))

    "t998".toCharArray.foreach(c => scanner.scan(c))

    val tokens = scanner.generateTokens()
    tokens should have length 1
    tokens should contain(TerminalToken("t998", TokenKind.Id))
  }

  it should "reject non-tokenizable (9112abc) inputs" in {
    val scanner = Scanner(Dfa(TokenKind.Id.getRegexp()))

    intercept[ScanningException] {
      "9122abc".toCharArray.foreach(c => scanner.scan(c))
      scanner.generateTokens()
    }
  }

  /*
  Things we want to test:
  Identifier
  Keyword
  Literal
  Separator
  Operator
  comment ??
*/
  //Identifiers
  "Scanner" should "recognize valid IDs" in {
    val test_ids = Seq[String]("String", "i3", "MAX_VALUE", "isLetterOrDigit")
    test_ids.map(
      id => {
        val scanner = Scanner(joosDfa)
        id.toCharArray.foreach(c => scanner.scan(c))
        val tokens = scanner.generateTokens()
        tokens should have length 1
        tokens should contain(TerminalToken(id, TokenKind.Id))
      }
    )
  }

  it should "recognize all valid keywords" in {
    val test_keywords = Set[String](
      "abstract", "default", "if", "private", "this", "boolean", "do",
      "implements", "protected", "throw", "break", "double", "import", "public", "throws", "byte", "else",
      "instanceof", "return", "transient", "case", "extends", "int", "short", "try", "catch", "final",
      "interface", "static", "void", "char", "finally", "long", "strictfp", "volatile", "class", "float",
      "native", "super", "while", "const", "for", "new", "switch", "continue", "goto", "package", "synchronized"
    )

    val scanner = Scanner(joosDfa)
    var counter = 1

    TokenKind.values.map(
      value => {
        val token_kind_value = value.asInstanceOf[TokenKindValue]
        val keyword = token_kind_value.getName().toLowerCase
        if (test_keywords.contains(keyword)) {
          keyword.toCharArray.foreach(c => scanner.scan(c))
          val tokens = scanner.generateTokens()
          tokens should have length counter
          counter += 1
          tokens should contain(TerminalToken(keyword, value))
        }
      }
    )
  }

  it should "recognize all separators" in {
    val separators =
      Map[String, TokenKindValue](
        "(" -> TokenKind.LeftParen,
        ")" -> TokenKind.RightParen,
        "{" -> TokenKind.LeftBrace,
        "}" -> TokenKind.RightBrace,
        "[" -> TokenKind.LeftBracket,
        "]" -> TokenKind.RightBracket,
        ";" -> TokenKind.SemiColon,
        "," -> TokenKind.Comma,
        "." -> TokenKind.Dot
      )
    val scanner = Scanner(joosDfa)
    var counter = 1

    separators.keys.foreach(
      sep => {
        sep.toCharArray.foreach(c => scanner.scan(c))
        val tokens = scanner.generateTokens()
        tokens should have length counter
        counter += 1
        tokens should contain(TerminalToken(sep, separators(sep)))
      }
    )
  }

  it should "recognize valid IntegerLiterals" in {
    val integers =
      Map[String, TokenKindValue](
        "0" -> TokenKind.DecimalIntLiteral,
        "2" -> TokenKind.DecimalIntLiteral,
        "0372" -> TokenKind.OctalIntLiteral,
        "0xDadaCafe" -> TokenKind.HexIntLiteral,
        "1996" -> TokenKind.DecimalIntLiteral,
        "0x00FF00FF" -> TokenKind.HexIntLiteral,
        "0l" -> TokenKind.DecimalLongLiteral,
        "0x100000000L" -> TokenKind.HexIntLiteral,
        "2147483648L" -> TokenKind.DecimalLongLiteral,
        "0xC0B0L" -> TokenKind.HexIntLiteral
      )
    val scanner = Scanner(joosDfa)
    var counter = 1

    integers.keys.foreach(
      sep => {
        sep.toCharArray.foreach(c => scanner.scan(c))
        val tokens = scanner.generateTokens()
        tokens should have length counter
        counter += 1
        tokens should contain(TerminalToken(sep, integers(sep)))
      }
    )
  }

  it should "recognize floating point values" in {
    val floating_points =
      Map[String, TokenKindValue](
        "1e1f" -> TokenKind.FloatingPointLiteral,
        "2.f" -> TokenKind.FloatingPointLiteral,
        ".3f" -> TokenKind.FloatingPointLiteral,
        "0f" -> TokenKind.FloatingPointLiteral,
        "3.14f" -> TokenKind.FloatingPointLiteral,
        "6.022137e+23f" -> TokenKind.FloatingPointLiteral,
        "1e1" -> TokenKind.FloatingPointLiteral,
        "2." -> TokenKind.FloatingPointLiteral,
        ".3" -> TokenKind.FloatingPointLiteral,
        "0.0" -> TokenKind.FloatingPointLiteral,
        "3.14" -> TokenKind.FloatingPointLiteral,
        "1e-9d" -> TokenKind.FloatingPointLiteral,
        "1e137" -> TokenKind.FloatingPointLiteral
      )
    val scanner = Scanner(joosDfa)
    var counter = 1

    floating_points.keys.foreach(
      num => {
        num.toCharArray.foreach(c => scanner.scan(c))
        val tokens = scanner.generateTokens()
        tokens should have length counter
        counter += 1
        tokens should contain(TerminalToken(num, floating_points(num)))
      }
    )
  }

  it should "recognize boolean literals" in {
    val floating_points =
      Map[String, TokenKindValue](
        "true" -> TokenKind.True,
        "false" -> TokenKind.False
      )
    val scanner = Scanner(joosDfa)
    var counter = 1

    floating_points.keys.foreach(
      num => {
        num.toCharArray.foreach(c => scanner.scan(c))
        val tokens = scanner.generateTokens()
        tokens should have length counter
        counter += 1
        tokens should contain(TerminalToken(num, floating_points(num)))
      }
    )
  }

  it should "recognize character literals" in {
    val characters =
      Map[String, TokenKindValue](
        "'a'" -> TokenKind.CharacterLiteral,
        "'%'" -> TokenKind.CharacterLiteral,
        "'\\t'" -> TokenKind.CharacterLiteral,
        "'\\\\'" -> TokenKind.CharacterLiteral,
        "'\\''" -> TokenKind.CharacterLiteral,
        "'\\u03a9'" -> TokenKind.CharacterLiteral,
        "'\\uFFFF'" -> TokenKind.CharacterLiteral,
        "'\\177'" -> TokenKind.CharacterLiteral
      )

    val scanner = Scanner(joosDfa)
    var counter = 1

    characters.keys.foreach(
      char => {
        char.toCharArray.foreach(c => scanner.scan(c))
        val tokens = scanner.generateTokens()
        tokens should have length counter
        counter += 1
        tokens should contain(TerminalToken(char, characters(char)))
      }
    )
  }
}
