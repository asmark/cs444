package joos.preprocessor

import joos.preprocessor.tasks.parser.ActionTableGeneratorTask
import joos.preprocessor.tasks.scanner.DfaGeneratorTask
import joos.resources
import joos.core.Logger

object Runner {
  def main(arguments: Array[String]) {
    Logger.logInformation("Running preprocessor...")
    resources.clean()
    Seq(DfaGeneratorTask, ActionTableGeneratorTask).foreach(_.runTask())
    Logger.logInformation("Preprocessor done")
  }
}
