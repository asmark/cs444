import sbt._
import Keys._

object Joos1WCompilerBuild extends Build {

  def getFilePath(baseDir : File, fileName : String) : File = {
    return baseDir / GeneratorConstants.GeneratedPackage / fileName
  }

  lazy val project = Project(
    id = "root",
    base = file("."),
    settings = Defaults.defaultSettings ++ Seq(
//      (sourceGenerators in Compile) <+= (sourceManaged in Compile) map { dir =>
      	// Auto generate code here
//      }
    )
  )
}
