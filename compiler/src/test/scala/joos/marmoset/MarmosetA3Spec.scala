package joos.marmoset

import joos.compiler.CompilationException
import joos.core.Logger
import joos.semantic.{TypeChecking, NameResolution}
import joos.syntax.SyntaxCheck
import joos.test.tags.IntegrationTest
import org.scalatest.{Matchers, FlatSpec}

class MarmosetA3Spec extends FlatSpec with Matchers {

  val assignmentNumber = 3
  val standardLibrary = getStandardLibrary(assignmentNumber).flatMap(getJavaFiles)

  behavior of "Name resolution of valid joos"
  getValidTestCases(assignmentNumber).foreach {
    testCase => it should s"accept ${testCase.getName}" taggedAs IntegrationTest in {
        val files = getJavaFiles(testCase) ++ standardLibrary
        val asts = files map SyntaxCheck.apply
        NameResolution(asts)
        TypeChecking(asts)
    }
  }

  behavior of "Name resolution of invalid joos"
  getInvalidTestCases(assignmentNumber).foreach {
    testCase => it should s"reject ${testCase.getName}" taggedAs IntegrationTest in {
//      if (testCase.getName.equals("Je_6_Assignable_Condition")) {
      val files = getJavaFiles(testCase) ++ standardLibrary
      Logger.logInformation(
        intercept[CompilationException] {
          val asts = files map SyntaxCheck.apply
          NameResolution(asts)
          TypeChecking(asts)
        }.getMessage)
//      }
    }
  }
}
