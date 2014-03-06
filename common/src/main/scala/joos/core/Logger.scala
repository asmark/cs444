package joos.core

object Logger {
  private[this] val LogOff = Int.MaxValue
  private[this] val LogError = 3
  private[this] val LogWarning = 2
  private[this] val LogInformation = 1
  private[this] val LogLevel = LogInformation

  private def prefixWith(prefix: String, original: String) = {
    original.split("[\\r\\n]").foldLeft(Seq.empty[String]) {
      (lines : Seq[String], line: String) =>
          lines :+ s"${prefix} ${line}"
    }.mkString("\r\n")
  }

  def logError(text: String): this.type = {
    if (LogLevel <= LogError) {
      Console.err.println(s"${Console.RED} ${prefixWith("[ERROR]", text)} ${Console.RESET}")
    }
    this
  }

  def logWarning(text: String): this.type = {
    if (LogLevel <= LogWarning) {
      Console.err.println(s"${Console.YELLOW} ${prefixWith("[WARN]", text)} ${Console.RESET}")
    }
    this
  }

  def logInformation(text: String): this.type = {
    if (LogLevel <= LogInformation) {
      Console.err.println(s"${Console.BLUE} ${prefixWith("[INFO]", text)} ${Console.RESET}")
    }
    this
  }
}
