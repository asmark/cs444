package joos.preprocessor

import joos.preprocessor.tasks.DfaGeneratorTask

object Runner {
  def main(arguments: Array[String]) {
    arguments.foreach(println)
    println("Hello World")

    DfaGeneratorTask.runTask()
  }
}
