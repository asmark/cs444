package joos.marmoset

import joos.compiler.CompilationException
import joos.core.Logger
import joos.semantic.{StaticAnalysis, TypeChecking, NameResolution}
import joos.syntax.SyntaxCheck
import joos.test.tags.IntegrationTest
import org.scalatest.{Matchers, FlatSpec}

class MarmosetA3Spec extends FlatSpec with Matchers {

  val assignmentNumber = 3
  val standardLibrary = getStandardLibrary(assignmentNumber).flatMap(getJavaFiles)

  behavior of "Type checking of valid joos"
  getValidTestCases(assignmentNumber).foreach {
    testCase => it should s"accept ${testCase.getName}" taggedAs IntegrationTest in {
      val files = getJavaFiles(testCase) ++ standardLibrary
      val asts = files map SyntaxCheck.apply
      NameResolution(asts)
      TypeChecking(asts)
      StaticAnalysis(asts)
    }
  }

  behavior of "Type checking of invalid joos"
  getInvalidTestCases(assignmentNumber).foreach {
    testCase => it should s"reject ${testCase.getName}" taggedAs IntegrationTest in {
      val files = getJavaFiles(testCase) ++ standardLibrary
      Logger.logInformation(
        intercept[CompilationException] {
          val asts = files map SyntaxCheck.apply
          NameResolution(asts)
          TypeChecking(asts)
        }.getMessage)
    }
  }
}
