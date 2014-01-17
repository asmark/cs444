name := "Joos1W SBT"

version := "1.0"

scalaVersion := "2.10.3"

// Dependencies
//sourceDirectory in Compile <<= Seq(baseDirectory(_ / "gen" / "src")

unmanagedSourceDirectories in Compile <++= baseDirectory { base =>
  Seq(base / "gen" / "src", base)
}

resourceDirectory in Compile <<= baseDirectory(_ / "gen" / "resources")

