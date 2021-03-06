import sbt._
import sbt.Keys._
import sbtassembly.Plugin._
import AssemblyKeys._
import ScoverageSbtPlugin._

object Dependencies {
  val scalaTest = "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
  val apacheCommon = "commons-lang" % "commons-lang" % "2.6"
}

object Joos1wCompilerBuild extends Build {

  val generatedResourceDirectory = "generated-resources"

  val commonSettings = Defaults.defaultSettings ++ Seq(
    crossPaths := false,
    fork in run := false,
    fork in test := false,
    parallelExecution in Test := false,
    testOptions in testQuick in Test += Tests.Argument("-l", "joos.test.tags.IntegrationTest"),
    version := "5.0.0",
    scalaVersion := "2.10.3",
    scalacOptions := Seq(
      // Turn on all warnings
      "-feature",
      "-deprecation",
      "-unchecked",
      "-target:jvm-1.6",
      "-encoding", "utf8",
      "-language:postfixOps",
      "-language:implicitConversions"
    ),
    libraryDependencies ++= Seq(
      Dependencies.scalaTest,
      Dependencies.apacheCommon
    )
  ) ++ assemblySettings ++ Seq(
    test in assembly := {}
  ) ++ instrumentSettings

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
      unmanagedResourceDirectories in Compile += baseDirectory.value / generatedResourceDirectory,
      resourceGenerators in Compile <+= Def.task {
        // Generate a build.properties that the preprocessor can use
        val managedResourceDirectory = (resourceManaged in Compile).value
        val file = managedResourceDirectory / "build.properties"
        val properties = Map(
          "generated-resource-directory"
            -> (baseDirectory.value / generatedResourceDirectory).getPath.replace('\\', '/')
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

  lazy val semanticAnalyzer = Project(
    id = "semantic-analyzer",
    base = file("semantic-analyzer"),
    settings = commonSettings
  ) dependsOn(common, parser)

  lazy val staticAnalyzer = Project(
    id = "static-analyzer",
    base = file("static-analyzer"),
    settings = commonSettings
  ) dependsOn(common)

  lazy val codeGenerator = Project(
    id = "code-generator",
    base = file("code-generator"),
    settings = commonSettings
  ) dependsOn(common, semanticAnalyzer)

  lazy val compiler = Project(
    id = "compiler",
    base = file("compiler"),
    settings = commonSettings ++ Seq(
      description := "Joos 1W Compiler",
      jarName in assembly := "compiler.jar",
      mainClass in assembly := Some("joos.compiler.Compiler")
    )
  ) dependsOn(common, preprocessor, scanner, parser, semanticAnalyzer, staticAnalyzer, codeGenerator)

  lazy val project = Project(
    id = "cs-444",
    base = file("."),
    settings = commonSettings
  ) aggregate(compiler, common, preprocessor, scanner, parser, semanticAnalyzer, staticAnalyzer, codeGenerator)
}
