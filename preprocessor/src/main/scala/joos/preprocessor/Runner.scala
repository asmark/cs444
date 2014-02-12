package joos.preprocessor

import joos.preprocessor.tasks.parser.ActionTableGeneratorTask
import joos.preprocessor.tasks.scanner.DfaGeneratorTask
import joos.resources

object Runner {
  def main(arguments: Array[String]) {
    resources.clean()
    println("Hello World")

    Seq(DfaGeneratorTask, ActionTableGeneratorTask).foreach(_.runTask())
  }
}
