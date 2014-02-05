import sbt._
import sbt.Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object Dependencies {
  val scalaTest = "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
  val guava = "com.google.guava" % "guava" % "r09"
}

object Joos1wCompilerBuild extends Build {

  final val HumanGrammar = "joos-1w-grammar.txt"
  final val MachineGrammar = "joos-1w-grammar.cfg"
  final val Dfa = "joos-1w-dfa.dfa"
  final val ActionTable = "joos-1w-action-table.txt"
  final val LrOneGrammar = "joos-1w-grammar.lr1"

  val commonSettings = Defaults.defaultSettings ++ Seq(
    crossPaths := false,
    fork in run := true,
    version := "1.0.0",
    scalaVersion := "2.10.3",
    scalacOptions := Seq(
      // Turn on all warnings
      "-feature",
      "-deprecation",
      "-unchecked",
      "-target:jvm-1.6",
      "-encoding", "utf8"
    ),
    libraryDependencies ++= Seq(
      Dependencies.scalaTest,
      Dependencies.guava
    )
  ) ++ assemblySettings ++ Seq(
    assemblyOption in assembly ~= { _.copy(includeScala = false) }
  )

  // Codes shared across multiple components
  lazy val common = Project(
    id = "common",
    base = file("common"),
    settings = commonSettings
  )

  lazy val preprocessor: Project = Project(
    id = "preprocessor",
    base = file("preprocessor"),
    settings = commonSettings ++ Seq(
      resourceGenerators in Compile <+= Def.task {
        // Generate a build.properties that the preprocessor can use
        val managedResourceDirectory = (resourceManaged in Compile).value
        val file = managedResourceDirectory / "build.properties"
        val properties = Map(
          "managed-resource-directory" -> managedResourceDirectory.getPath.replace('\\', '/'),
          "grammar" -> HumanGrammar,
          "machine-grammar" -> MachineGrammar,
          "dfa" -> Dfa,
          "action-table" -> ActionTable,
          "lr-one-grammar" -> LrOneGrammar
        )

        val builder = new StringBuilder()
        for ((key, value) <- properties) {
          builder
            .append(key)
            .append('=')
            .append(value)
            .append('\n')
        }

        IO.write(file, builder.toString())
        Seq(file)
      }
    )
  ) dependsOn (common)

  lazy val scanner = Project(
    id = "scanner",
    base = file("scanner"),
    settings = commonSettings
  ) dependsOn(common, preprocessor)

  lazy val parser = Project(
    id = "parser",
    base = file("parser"),
    settings = commonSettings
  ) dependsOn(common, scanner, preprocessor)

  lazy val compiler = Project(
    id = "compiler",
    base = file("compiler"),
    settings = commonSettings ++ Seq(
      description := "Joos 1W Compiler"
    )
  ) dependsOn(common, preprocessor, scanner, parser)

  lazy val project = Project(
    id = "cs-444",
    base = file("."),
    settings = commonSettings
  ) aggregate(compiler, common, preprocessor, scanner, parser)

}
