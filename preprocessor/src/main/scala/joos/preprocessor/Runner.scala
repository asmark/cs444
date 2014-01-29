package joos.preprocessor

import java.io.{BufferedOutputStream, BufferedInputStream, FileInputStream, FileOutputStream}
import java.util.Properties
import joos.language.ContextFreeGrammar
import joos.preprocessing.ActionTableGenerator
import joos.preprocessor.tasks.DfaGeneratorTask

object Runner {
  def main(arguments: Array[String]) {
    arguments.foreach(println)
    println("Hello World")

    //DfaGeneratorTask.runTask()
    val prop = new Properties()
    val in = getClass().getResourceAsStream("/build.properties")
    prop.load(in)

    val inputStream = getClass().getResourceAsStream('/' + prop.getProperty("grammar"))
    val grammar = ContextFreeGrammar.fromHumanReadableFormat(inputStream)
    val outputStream = new FileOutputStream(prop.getProperty("machine-grammar"))
    grammar.toMachineReadableFormat(outputStream)
    val tableGenerator = new ActionTableGenerator
    tableGenerator
      .createActionTable(
      new BufferedInputStream(new FileInputStream(prop.getProperty("machine-grammar"))),
      new BufferedOutputStream(new FileOutputStream("action-table.txt"))
    )

    in.close()
  }
}
