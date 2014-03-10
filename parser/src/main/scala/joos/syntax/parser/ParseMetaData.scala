package joos.syntax.parser

import java.io.File
import java.util.regex.Pattern
import joos.syntax.weeder.WeederException

class ParseMetaData(val fileName: String)

object ParseMetaData {

  final lazy val javaFilePattern = Pattern.compile("([A-Za-z][0-9a-zA-Z\\$_]*).(java|java.test)$")

  def apply(filePath: String) = {
    val fileNameWithExt = new File(filePath).getName
    val fileMatcher = javaFilePattern.matcher(fileNameWithExt)
    if (!fileMatcher.find()) {
      throw new WeederException(s"File name (${fileNameWithExt}) must end in .java")
    }
    new ParseMetaData(fileMatcher.group(1))
  }
}