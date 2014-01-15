import sbt._
import Keys._

object Joos1WCompilerBuild extends Build {

  def getFilePath(baseDir : File, fileName : String) : File = {
    return baseDir / GeneratorConstants.GENERATED_PACKAGE / fileName
  }

  def generateTokenKinds(dir : File) : File = {
    val file = getFilePath(dir, "TokenKind.scala")
    IO.write(file, TokenKindGenerator.generate())
    return file
  }

  lazy val project = Project(
    id = "root",
    base = file("."),
    settings = Defaults.defaultSettings ++ Seq(
      (sourceGenerators in Compile) <+= (sourceManaged in Compile) map { dir =>
      	// Auto generate TokenKinds
        Seq(generateTokenKinds(dir))
      }
    )
  )
}
