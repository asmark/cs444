package joos.marmoset

import joos.syntax.SyntaxCheck
import joos.test.tags.IntegrationTest
import org.scalatest.{Matchers, FlatSpec}
import scala.io.Source

class MarmosetA1Spec extends FlatSpec with Matchers {

  final val validJoos = "/a1/marmoset/valid"
  final val invalidJoos = "/a1/marmoset/invalid"

  def getSource(dir: String) = Source.fromURL(getClass.getResource(dir))

  behavior of "Parsing valid joos"
  getSource(validJoos).getLines().foreach {
    file =>
      it should s"accept ${file}" taggedAs IntegrationTest in {
        val filePath = getClass.getResource(validJoos + "/" + file).getPath
        val result = SyntaxCheck(filePath)
        result shouldNot be(None)
      }
  }

  behavior of "Parsing invalid joos"
  getSource(invalidJoos).getLines().foreach {
    file =>
      it should s"reject ${file}" taggedAs IntegrationTest in {
        val filePath = getClass.getResource(invalidJoos + "/" + file).getPath
        val result = SyntaxCheck(filePath)
        result shouldBe None
      }
  }
}
