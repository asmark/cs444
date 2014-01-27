package joos.preprocessor

import _root_.joos.preprocessor.tasks.DfaGeneratorTask

object Runner {
  def main(arguments: Array[String]) {
    arguments.foreach(println)
    println("Hello World")

    if (!DfaGeneratorTask.isTaskCached())
      DfaGeneratorTask.executeTask()
  }
}
