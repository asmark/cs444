#!/bin/sh
exec scala "$0" "$@"
!#

import java.io.File
import scala.sys.process._

def validJoos(assignmentNumber: Int) = s"compiler/src/test/resources/a${assignmentNumber}/marmoset/valid"
def invalidJoos(assignmentNumber: Int) = s"compiler/src/test/resources/a${assignmentNumber}/marmoset/invalid"
def standardLibrary(assignmentNumber: Int) = s"compiler/src/test/resources/a${assignmentNumber}/marmoset/stdlib"

def getJavaFiles(testDirectory: File): Array[File] = {
    val these = testDirectory.listFiles()
    these.filterNot(_.isDirectory) ++ these.filter(_.isDirectory).flatMap(getJavaFiles)
}

def getValidTestCases(assignmentNumber: Int): Array[File] = {
    new File(validJoos(assignmentNumber)).listFiles()
}

def getInvalidTestCases(assignmentNumber: Int): Array[File] = {
    new File(invalidJoos(assignmentNumber)).listFiles()
}

def getStandardLibrary(assignmentNumber: Int): Array[File] = {
    new File(standardLibrary(assignmentNumber)).listFiles()
}

for (i <- 4 to 4) {
    val standardLibrary = getStandardLibrary(i).flatMap(getJavaFiles)
    getValidTestCases(i).foreach {
        testCase =>
            println(s"Testing ${testCase}")
            val files = getJavaFiles(testCase) ++ standardLibrary
            val arguments = files.mkString(" ")
            val status = s"./joosc ${arguments}".!
            if (status != 0) {
                println(s"${testCase} failed")
                sys.exit(status)
            }
    }

    getInvalidTestCases(i).foreach {
        testCase =>
            println(s"Testing ${testCase}")
            val files = getJavaFiles(testCase) ++ standardLibrary
            val arguments = files.mkString(" ")
            val status = s"./joosc ${arguments}".!
            if (status != 42) {
                println(s"${testCase} failed")
                sys.exit(status)
            }
    }
}
