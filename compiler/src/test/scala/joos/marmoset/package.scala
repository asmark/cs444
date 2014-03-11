package joos

import java.io.File

package object marmoset {

  private def validJoos(assignmentNumber: Int) = s"/a${assignmentNumber}/marmoset/valid"
  private def invalidJoos(assignmentNumber: Int) = s"/a${assignmentNumber}/marmoset/invalid"
  private def standardLibrary(assignmentNumber: Int) = s"/a${assignmentNumber}/marmoset/stdlib"

  private def resourceToPath(resource: String) = {
    getClass.getResource(resource).getPath
  }

  def getJavaFiles(testDirectory: File): Array[File] = {
    val these = testDirectory.listFiles()
    these.filterNot(_.isDirectory) ++ these.filter(_.isDirectory).flatMap(getJavaFiles)
  }

  def getValidTestCases(assignmentNumber: Int): Array[File] = {
    (new File(resourceToPath(validJoos(assignmentNumber)))).listFiles()
  }

  def getInvalidTestCases(assignmentNumber: Int): Array[File] = {
    (new File(resourceToPath(invalidJoos(assignmentNumber)))).listFiles()
  }

  def getStandardLibrary(assignmentNumber: Int): Array[File] = {
    (new File(resourceToPath(standardLibrary(assignmentNumber)))).listFiles()
  }

}
