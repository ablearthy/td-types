ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

val circeVersion = "0.14.1"

lazy val root = (project in file("."))
  .settings(
    name := "td-tl-parser",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.3.3",
    libraryDependencies += "org.scalameta" %% "scalameta" % "4.7.5",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion)
  )
