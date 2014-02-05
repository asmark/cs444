package joos.preprocessor

import joos.preprocessor.tasks.parser.LrOneGeneratorTask
import joos.preprocessor.tasks.scanner.DfaGeneratorTask

object Runner {
  def main(arguments: Array[String]) {
    arguments.foreach(println)
    println("Hello World")

    val tasks = Seq(DfaGeneratorTask, LrOneGeneratorTask)
    tasks.foreach(_.runTask())
  }
}
