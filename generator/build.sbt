ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "io.github.ablearthy"


val circeVersion = "0.14.1"

lazy val root = (project in file("."))
  .enablePlugins(SbtPlugin)
  .settings(
    name := "td-tl-generator",
    sbtPlugin := true,
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.3.3",
    libraryDependencies += "org.scalameta" %% "scalameta" % "4.7.5",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser",
      "io.circe" %% "circe-shapes",
      "io.circe" %% "circe-generic-extras"
    ).map(_ % circeVersion)
)
