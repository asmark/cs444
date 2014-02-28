package joos.a2

import java.io.File
import org.scalatest.{Matchers, FlatSpec}
import scala.io.Source
import joos.a1.SyntaxCheck
import joos.ast.{CompilationUnit, AbstractSyntaxTree}

class MarmosetA2Spec extends FlatSpec with Matchers {

  final val validJoos = "/a2/marmoset/valid"
  final val invalidJoos = "/a2/marmoset/invalid"

  def getJavaFiles(dir: File): Array[File] = {
    val these = dir.listFiles()
    these.filterNot(_.isDirectory) ++ these.filter(_.isDirectory).flatMap(getJavaFiles)
  }

  def getTestCases(dir: String) = {
    new File(getClass.getResource(dir).getPath).listFiles()
  }

  behavior of "Name resolution of valid joos"
  getTestCases(validJoos).foreach {
    testCase => it should s"accept ${testCase.getName}" in {
        val files = getJavaFiles(testCase) map (_.getAbsolutePath)
        val asts = files flatMap SyntaxCheck.apply map AbstractSyntaxTree.apply
        // Do something with asts
        asts foreach (_.root shouldBe a[CompilationUnit])
      }
  }

  behavior of "Name resultion of invalid joos"
  getTestCases(invalidJoos).foreach {
    testCase => it should s"accept ${testCase.getName}" in {
      val files = getJavaFiles(testCase) map (_.getAbsolutePath)
      val asts = files flatMap SyntaxCheck.apply map AbstractSyntaxTree.apply
      // Do something with asts
      asts foreach (_.root shouldBe a[CompilationUnit])
    }
  }


}
