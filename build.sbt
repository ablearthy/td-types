import _root_.io.github.ablearthy.tl.GeneratorPlugin
import scala.math.Ordering.Implicits._

lazy val scala213 = "2.13.15"
lazy val scala3 = "3.5.2"

lazy val supportedScalaVersions = Seq(scala213, scala3)

val circeVersion = "0.14.1"

inThisBuild(
  List(
    organization := "io.github.ablearthy",
    homepage := Some(url("https://github.com/ablearthy/td-types")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "ablearthy",
        "Able Arthy",
        "ablearthy@gmail.com",
        url("https://github.com/ablearthy")
      )
    ),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/ablearthy/td-types"),
        "scm:git@github.com:ablearthy/td-types.git"
      )
    )
  )
)

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
sonatypeRepository := "https://s01.oss.sonatype.org/service/local"

lazy val root = (project in file("."))
  .enablePlugins(GeneratorPlugin)
  .settings(
    name := "td-types",
    scalaVersion := scala213,
    crossScalaVersions := supportedScalaVersions,
    versionScheme := Some("semver-spec"),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
    Compile / sourceGenerators += Def.task {
      val tlFile = file(".") / "tl" / "td_api.tl"
      val path = (Compile / sourceManaged).value / "io" / "github" / "ablearthy" / "tl"
      GeneratorPlugin.generateTypes(tlFile, path)
    }.taskValue,
    Compile / scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, n)) => List("-Xmax-inlines:100000")
        case _            => Nil
      }
    }
  )
