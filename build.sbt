name := "FolderDiff"

version := "0.1"

scalaVersion := "2.11.1"

EclipseKeys.eclipseOutput := Some("target")

EclipseKeys.withSource := true

site.settings

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.0" % "test" withSources(),
    "org.scalacheck" %% "scalacheck" % "1.11.4" % "test" withSources()
)
