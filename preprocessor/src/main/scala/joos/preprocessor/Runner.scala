package joos.preprocessor

import joos.preprocessor.tasks.parser.ActionTableGeneratorTask
import joos.preprocessor.tasks.scanner.DfaGeneratorTask

object Runner {
  def main(arguments: Array[String]) {
    arguments.foreach(println)
    println("Hello World")

    Seq(DfaGeneratorTask, ActionTableGeneratorTask).foreach(_.runTask())
  }
}
