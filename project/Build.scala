import sbt._
import Keys._

import joos.lexer

object Dependencies {
  val scalaTest = "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
}

object Joos1wCompilerBuild extends Build {

  val commonSettings = Defaults.defaultSettings ++ Seq(
    version := "1.0-SNAPSHOT",
    scalaVersion := "2.10.3",
    scalacOptions := Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-target:jvm-1.6",
      "-encoding", "utf8"
    ),
    libraryDependencies ++= Seq(
      Dependencies.scalaTest
    )
  )

  def getFilePath(baseDir : File, fileName : String) : File = {
    return baseDir / GeneratorConstants.GeneratedPackage / fileName
  }

  lazy val common = Project(
    id = "common",
    base = file("common"),
    settings = commonSettings ++ Seq(

    )
  )

  lazy val scanner = Project(
    id = "scanner",
    base = file("scanner"),
    settings = commonSettings ++ Seq(

    )
  )

  lazy val project = Project(
    id = "root",
    base = file("."),
    settings = commonSettings
  ) aggregate(common, scanner)
}
