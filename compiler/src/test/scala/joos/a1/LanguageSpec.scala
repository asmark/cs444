package joos.a1

import org.scalatest.{Matchers, FlatSpec}
import scala.io.Source

class LanguageSpec extends FlatSpec with Matchers {

  final val joosLang = "/language/joos"
  final val javaLang = "/language/java"

  def getSource(dir: String) = Source.fromURL(getClass.getResource(dir))

  def getSource(dir: String, file: String) = Source.fromURL(getClass.getResource(dir + "/" + file))

  behavior of "Parsing Joos language features"
  getSource(joosLang).getLines().foreach {
    file =>
      it should s"handle joos language feature in ${file}" in {
        val filePath = getClass.getResource(joosLang + "/" + file).getPath
        val result = ScanParseWeed(filePath)
        result shouldEqual 0
      }
  }

  behavior of "Parsing java language features (not in Joos)"
  getSource(javaLang).getLines().foreach {
    file =>
      it should s"reject java language feature ${file}" in {
        val filePath = getClass.getResource(javaLang + "/" + file).getPath
        val result = ScanParseWeed(filePath)
        result shouldEqual 42
      }
  }
}
