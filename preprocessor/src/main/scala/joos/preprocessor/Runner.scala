package joos.preprocessor

import java.io._
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
    val machineGrammarFile = new File(prop.getProperty("managed-resource-directory"), prop.getProperty("machine-grammar"))
    val outputStream = new FileOutputStream(machineGrammarFile)
    grammar.toMachineReadableFormat(outputStream)
    val tableGenerator = new ActionTableGenerator
    tableGenerator
      .createActionTable(
      new BufferedInputStream(new FileInputStream(machineGrammarFile)),
      new BufferedOutputStream(new FileOutputStream(new File(prop.getProperty("managed-resource-directory"), "action-table.txt")))
    )

    // Create .lr1 file
    val cfgFile = new File(prop.getProperty("managed-resource-directory"), prop.getProperty("machine-grammar"))
    val grammarFile =  new File(prop.getProperty("managed-resource-directory"), "grammar.lr1")

    new FileOutputStream(grammarFile) getChannel() transferFrom(
      new FileInputStream(cfgFile) getChannel, 0, Long.MaxValue)

    val actionTableFile = new File(prop.getProperty("managed-resource-directory"), "action-table.txt")
    new FileOutputStream(grammarFile, true) getChannel() transferFrom(
      new FileInputStream(actionTableFile) getChannel, 0, Long.MaxValue)

    in.close()
  }
}
