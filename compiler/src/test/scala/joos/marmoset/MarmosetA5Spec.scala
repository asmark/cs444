package joos.marmoset

import joos.semantic.{CodeGeneration, StaticAnalysis, TypeChecking, NameResolution}
import joos.syntax.SyntaxCheck
import joos.test.tags.IntegrationTest
import org.scalatest.{Matchers, FlatSpec}

class MarmosetA5Spec extends FlatSpec with Matchers {

  val assignmentNumber = 5
  val standardLibrary = getStandardLibrary(assignmentNumber).flatMap(getJavaFiles)

  behavior of "Code Generation of valid joos"
  getValidTestCases(assignmentNumber).foreach {
    testCase => it should s"accept ${testCase.getName}" taggedAs IntegrationTest in {
      val files = getJavaFiles(testCase) ++ standardLibrary
      val asts = files map SyntaxCheck.apply
      NameResolution(asts)
      TypeChecking(asts)
      StaticAnalysis(asts)
      CodeGeneration(asts)
    }
  }
}
