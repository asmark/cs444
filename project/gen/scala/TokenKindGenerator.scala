import treehugger.forest._
import treehuggerDSL._
import scala.util.parsing.json.JSON
import scala.io.Source

object TokenKindGenerator {
  private val KEYWORD_FILE = "/keywords.json"
  private val TOKEN_KIND_CLASS_NAME = "TokenKind"

  def generate() : String  = {
    val keywordsResource = Source.fromURL(getClass.getResource(KEYWORD_FILE)).mkString
    val keywords: Map[String, Any] = JSON.parseFull(keywordsResource).get.asInstanceOf[Map[String, Any]]

    var tokenDefs: Array[Tree] = Array(TYPEVAR(TOKEN_KIND_CLASS_NAME) := REF("Value"))
    keywords.foreach {
      pair =>
        tokenDefs = tokenDefs :+ (VAL(pair._1) := REF("Value") APPLY LIT(pair._2))
    }

    val TokenKindCodeTree = BLOCK(
      OBJECTDEF(TOKEN_KIND_CLASS_NAME) withParents ("Enumeration") := BLOCK(tokenDefs)
    ) inPackage(GeneratorConstants.GENERATED_PACKAGE)

    return treeToString(TokenKindCodeTree)
  }
}
