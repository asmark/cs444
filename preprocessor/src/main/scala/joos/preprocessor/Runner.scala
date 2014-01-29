package joos.preprocessor

import joos.preprocessor.tasks.DfaGeneratorTask
import java.io.{FileOutputStream}
import java.util.Properties
import joos.language.ContextFreeGrammar

object Runner {
  def main(arguments: Array[String]) {
    arguments.foreach(println)
    println("Hello World")

    DfaGeneratorTask.runTask()
    val prop = new Properties()
    val in = getClass().getResourceAsStream("/build.properties")
    prop.load(in)

    val inputStream = getClass().getResourceAsStream('/' + prop.getProperty("grammar"))
    val grammar = ContextFreeGrammar.fromHumanReadableFormat(inputStream)
    val outputStream = new FileOutputStream(prop.getProperty("machine-grammar"))
    grammar.toMachineReadableFormat(outputStream)

    in.close()
  }
}
