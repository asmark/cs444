package joos.marmoset

import java.io.File
import joos.semantic.{NameResolution}
import joos.syntax.SyntaxCheck
import joos.test.tags.IntegrationTest
import org.scalatest.{Matchers, FlatSpec}
import joos.compiler.CompilationException

class MarmosetA2Spec extends FlatSpec with Matchers {

  final val validJoos = "/a2/marmoset/valid"
  final val invalidJoos = "/a2/marmoset/invalid"
  final val standardLibrary = getJavaFiles(new File(this.getClass.getResource("/a2/marmoset/stdlib").getPath))

  def getJavaFiles(dir: File): Array[File] = {
    val these = dir.listFiles()
    these.filterNot(_.isDirectory) ++ these.filter(_.isDirectory).flatMap(getJavaFiles)
  }

  def getTestCases(dir: String) = (new File(getClass.getResource(dir).getPath)).listFiles()

  behavior of "Name resolution of valid joos"
  getTestCases(validJoos).foreach {
    testCase => it should s"accept ${testCase.getName}" taggedAs IntegrationTest in {
      val files = getJavaFiles(testCase) ++ standardLibrary map (_.getAbsolutePath)
      val asts = files map SyntaxCheck.apply
      NameResolution(asts)
    }
  }

  behavior of "Name resolution of invalid joos"
  getTestCases(invalidJoos).foreach {
    testCase => it should s"reject ${testCase.getName}" taggedAs IntegrationTest in {
      val files = getJavaFiles(testCase) ++ standardLibrary map (_.getAbsolutePath)

      intercept[CompilationException] {
        val asts = files map SyntaxCheck.apply
        NameResolution(asts)
      }
    }
  }
}
