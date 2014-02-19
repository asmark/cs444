package joos.a1

import org.scalatest.{Matchers, FlatSpec}
import scala.io.Source
import joos.ast.CompilationUnit

class MarmosetSpec extends FlatSpec with Matchers {

  final val validJoos = "/a1/marmoset/valid"
  final val invalidJoos = "/a1/marmoset/invalid"

  def getSource(dir: String) = Source.fromURL(getClass.getResource(dir))

  def getSource(dir: String, file: String) = Source.fromURL(getClass.getResource(dir + "/" + file))

  behavior of "Parsing valid joos"
  getSource(validJoos).getLines().foreach {
    file =>
      it should s"accept ${file}" in {
        val filePath = getClass.getResource(validJoos + "/" + file).getPath
        val result = SyntaxCheck(filePath)
        result shouldNot be(None)
        CompilationUnit(result.get.root)
      }
  }

  behavior of "Parsing invalid joos"
  getSource(invalidJoos).getLines().foreach {
    file =>
      it should s"reject ${file}" in {
        val filePath = getClass.getResource(invalidJoos + "/" + file).getPath
        val result = SyntaxCheck(filePath)
        result shouldBe (None)
      }
  }
}
