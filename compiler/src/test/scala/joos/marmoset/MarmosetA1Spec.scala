package joos.marmoset

import joos.syntax.{JoosSyntaxException, SyntaxCheck}
import joos.test.tags.IntegrationTest
import org.scalatest.{Matchers, FlatSpec}
import scala.io.Source
import joos.semantic.{StaticAnalysis, TypeChecking, NameResolution}
import joos.core.Logger
import joos.compiler.CompilationException

class MarmosetA1Spec extends FlatSpec with Matchers {

  val assignmentNumber = 1
  val standardLibrary = getStandardLibrary(assignmentNumber).flatMap(getJavaFiles)

  behavior of "Parsing valid joos"
  getValidTestCases(assignmentNumber).foreach {
    testCase => it should s"accept ${testCase.getName}" taggedAs IntegrationTest in {
      val files = getJavaFiles(testCase) ++ standardLibrary
      val asts = files map SyntaxCheck.apply
      NameResolution(asts)
      TypeChecking(asts)
      StaticAnalysis(asts)
    }
  }

  behavior of "Parsing invalid joos"
  getInvalidTestCases(assignmentNumber).foreach {
    testCase => it should s"reject ${testCase.getName}" taggedAs IntegrationTest in {
      val files = getJavaFiles(testCase) ++ standardLibrary

      Logger.logInformation(intercept[CompilationException] {
        files map SyntaxCheck.apply
      }.getMessage)
    }
  }
}
