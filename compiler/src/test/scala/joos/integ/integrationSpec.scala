package joos.integ

import joos.codegen.CodeGeneration
import joos.compiler.CompilationException
import joos.core.Logger
import joos.semantic.{StaticAnalysis, TypeChecking, NameResolution}
import joos.syntax.SyntaxCheck
import joos.test.tags.IntegrationTest
import org.scalatest.{FlatSpec, Matchers}

class integrationSpec extends FlatSpec with Matchers {

  val standardLibrary = getStandardLibrary.flatMap(getJavaFiles)

  behavior of "Compiling valid joos"
  getValidTestCases.foreach {
    testCase =>
      it should s"accept ${testCase.getName}" taggedAs IntegrationTest in {
        val files = getJavaFiles(testCase) ++ standardLibrary
        val asts = files map SyntaxCheck.apply
        NameResolution(asts)
        TypeChecking(asts)
        StaticAnalysis(asts)
        CodeGeneration(asts)
        Logger.logInformation(s"${testCase.getName} passed")
      }
  }

  behavior of "Compiling invalid joos"
  getInvalidTestCases.foreach {
    testCase =>
      it should s"reject ${testCase.getName}" taggedAs IntegrationTest in {
        val files = getJavaFiles(testCase) ++ standardLibrary

        Logger.logInformation(
        s"${testCase.getName}: " +
          intercept[CompilationException] {
            val asts = files map SyntaxCheck.apply
            NameResolution(asts)
            TypeChecking(asts)
            StaticAnalysis(asts)
            CodeGeneration(asts)
          }.getMessage
        )
      }
  }

}
