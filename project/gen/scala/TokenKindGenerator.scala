import treehugger.forest._
import treehuggerDSL._
import scala.util.parsing.json.JSON
import scala.io.Source

object TokenKindGenerator {
  private val KeywordFile = "/keywords.json"
  private val TokenKindObjectName = "TokenKind"

  def generate() : String  = {
    val keywordsResource = Source.fromURL(getClass.getResource(KeywordFile)).mkString
    val keywords: Map[String, Any] = JSON.parseFull(keywordsResource).get.asInstanceOf[Map[String, Any]]

    var tokenDefs: Array[Tree] = Array(TYPEVAR(TokenKindObjectName) := REF("Value"))
    keywords.foreach {
      pair =>
        tokenDefs = tokenDefs :+ (VAL(pair._1) := REF("Value") APPLY LIT(pair._2))
    }

    val tokenKindCodeTree = BLOCK(
      OBJECTDEF(TokenKindObjectName) withParents ("Enumeration") := BLOCK(tokenDefs)
    ) inPackage(GeneratorConstants.GeneratedPackage)

    return treeToString(tokenKindCodeTree)
  }
}
