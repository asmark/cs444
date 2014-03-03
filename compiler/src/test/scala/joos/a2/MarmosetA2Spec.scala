package joos.a2

import java.io.File
import joos.a1.SyntaxCheck
import joos.ast.AbstractSyntaxTree
import joos.semantic.SemanticException
import org.scalatest.{Matchers, FlatSpec}
import joos.analyzers.TypeEnvironmentBuilder

class MarmosetA2Spec extends FlatSpec with Matchers {

  final val validJoos = "/a2/marmoset/valid"
  final val invalidJoos = "/a2/marmoset/invalid"
  final val standardLibrary = getJavaFiles(new File(this.getClass.getResource("/a2/marmoset/stdlib").getPath))

  def getJavaFiles(dir: File): Array[File] = {
    val these = dir.listFiles()
    these.filterNot(_.isDirectory) ++ these.filter(_.isDirectory).flatMap(getJavaFiles)
  }

  def getTestCases(dir: String) = {
    (new File(getClass.getResource(dir).getPath)).listFiles()
  }

  behavior of "Name resolution of valid joos"
  getTestCases(validJoos).foreach {
    testCase => it should s"accept ${testCase.getName}" in {
      val files = getJavaFiles(testCase) ++ standardLibrary map (_.getAbsolutePath)
      val asts = files map SyntaxCheck.apply collect {
        case None => fail(s"Was not able to SyntaxCheck ${testCase.getName}")
        case Some(ast) => ast
      }
      SemanticCheck(asts)
    }
  }

  behavior of "Name resolution of invalid joos"
  getTestCases(invalidJoos).foreach {
    testCase => it should s"reject ${testCase.getName}" in {
      val files = getJavaFiles(testCase) ++ standardLibrary map (_.getAbsolutePath)

      intercept[SemanticException] {
        val asts = files map SyntaxCheck.apply collect {
          case None => fail(s"Was not able to SyntaxCheck ${testCase.getName}")
          case Some(ast) => ast
        }
        SemanticCheck(asts)
      }
    }
  }
}
