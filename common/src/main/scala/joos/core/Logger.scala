package joos.core

object Logger {
  private[this] val LogOff = Int.MaxValue
  private[this] val LogError = 3
  private[this] val LogWarning = 2
  private[this] val LogInformation = 1
  private[this] val LogLevel = LogInformation

  private def prefixWith(prefix: String, original: String) = {

    original.split("[\\r\\n]").foldLeft(new StringBuilder) {
      (buffer : StringBuilder, line : String) =>
        buffer.append(prefix).append(' ').append(line).append("\r\n")
    }
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
