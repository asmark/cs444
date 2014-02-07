package joos.parser

import java.io.File
import java.util.regex.Pattern
import joos.weeder.exceptions.WeederException

class ParseMetaData(val fileName: String)

object ParseMetaData {

  final lazy val javaFilePattern = Pattern.compile("([A-Za-z][0-9a-zA-Z$_]*).java")

  def apply(filePath: String) = {
    val fileNameWithExt = new File(filePath).getName
    val fileMatcher = javaFilePattern.matcher(fileNameWithExt)
    if (!fileMatcher.find()) {
      throw new WeederException("File name must end in .java")
    }
    new ParseMetaData(fileMatcher.group(1))
  }
}