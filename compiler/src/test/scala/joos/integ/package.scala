package joos

import java.io.File

package object integ {

  private val ValidTestDirectory = "/integ/valid"
  private val InvalidTestDirectory = "/integ/invalid"
  private val StandardLibraryDirectory = "/integ/stdlib"

  private def resourceToPath(resource: String) = {
    getClass.getResource(resource).getPath
  }

  def getJavaFiles(directory: File): Array[File] = {
    val these = directory.listFiles()
    these.filterNot(_.isDirectory) ++ these.filter(_.isDirectory).flatMap(getJavaFiles)
  }

  // TODO: Return expected output eventually for code generation
  def getValidTestCases: Array[File] = {
    (new File(resourceToPath(ValidTestDirectory))).listFiles().filter(_.getName.contains("BasicVariableReference"))
  }

  def getInvalidTestCases: Array[File] = {
    (new File(resourceToPath(InvalidTestDirectory))).listFiles()
  }

  def getStandardLibrary: Array[File] = {
    (new File(resourceToPath(StandardLibraryDirectory))).listFiles()
  }

}
