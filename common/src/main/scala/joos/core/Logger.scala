package joos.core

import java.io.{OutputStreamWriter, PrintWriter}

object Logger {
  private[this] val LogOff = Int.MaxValue
  private[this] val LogError = 3
  private[this] val LogWarning = 2
  private[this] val LogInformation = 1
  private[this] val LogLevel = LogInformation
  private[this] val writer = new PrintWriter(new OutputStreamWriter(System.err))

  private[this] def prefixWith(prefix: String, original: String) = {
    original.split("[\\r\\n]").foldLeft(Seq.empty[String]) {
      (lines : Seq[String], line: String) =>
          lines :+ s"${prefix} ${line}"
    }.mkString("\r\n")
  }

  def logError(text: String): this.type = {
    if (LogLevel <= LogError) {
      writer.println(s"${Console.RED} ${prefixWith("[ERROR]", text)} ${Console.RESET}")
      writer.flush()
    }
    this
  }

  def logWarning(text: String): this.type = {
    if (LogLevel <= LogWarning) {
      writer.println(s"${Console.YELLOW} ${prefixWith("[WARN]", text)} ${Console.RESET}")
      writer.flush()
    }
    this
  }

  def logInformation(text: String): this.type = {
    if (LogLevel <= LogInformation) {
      writer.println(s"${Console.BLUE} ${prefixWith("[INFO]", text)} ${Console.RESET}")
      writer.flush()
    }
    this
  }
}
