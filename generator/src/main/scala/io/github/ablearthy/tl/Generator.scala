package io.github.ablearthy.tl

import fastparse.{Parsed, parse}
import io.circe._
import io.circe.syntax._
import io.github.ablearthy.tl.gen.Gen
import io.github.ablearthy.tl.parser.schemaParser
import shapeless.{Coproduct, Generic}

import scala.meta._

import java.nio.file.{Paths, Files, Path}
import java.io.File
import java.io.InputStreamReader
import java.io.FileInputStream
import _root_.io.github.ablearthy.tl.parser.Definition

object Generator {

  def generateTypes(tlFile: File, basePath: File): Seq[File] = {
    val stream = new FileInputStream(tlFile)
    val result =
      parse(
        stream,
        schemaParser(_)
      ).get.value
    val gen = Gen(result.types)
    val t = writeTypes(gen, basePath.toPath.resolve("types"))
    val f = writeFunctions(gen, result.functions, basePath.toPath.resolve("functions"))
    t ++ f
  }

  private def writeTypes(gen: Gen, basePath: Path): Seq[File] = {
    import scala.collection.mutable.ArrayBuffer

    val generatedFiles = ArrayBuffer.empty[File]
    val pkgName = q"io.github.ablearthy.tl.types"

    val imports = List(
      q"import io.github.ablearthy.tl.aliases._"
    )

    Files.createDirectories(basePath)

    val sources = gen.groupedTypes.keys.map(x => gen.generate(x))
    sources.foreach { funit =>
      val fPath = basePath.resolve(s"${funit.fname}.scala")
      generatedFiles += fPath.toFile
      val source: Source = source"package $pkgName { ..$imports; ..${funit.source} }"
      Files.writeString(fPath, source.toString)
    }

    val encoders = sources.flatMap(_.encoders).toList
    val decoders = sources.flatMap(_.decoders).toList

    val objEnc = q"object encoders { ..$encoders }"
    val objDec = q"object decoders { ..$decoders }"

    val implicitImports = List(
      q"import io.github.ablearthy.tl.codecs._",
      q"import io.circe.Decoder",
      q"import io.circe.Encoder",
      q"import io.circe.generic.semiauto._",
      q"import cats.syntax.functor._"
    )
    val implicitSource = source"package $pkgName { ..$implicitImports; $objEnc; $objDec }"
    val implicitsPath = basePath.resolve("implicits.scala")
    Files.writeString(implicitsPath, implicitSource.toString)

    generatedFiles += implicitsPath.toFile
    generatedFiles.toSeq
  }

  private def writeFunctions(gen: Gen, defs: Vector[Definition], basePath: Path): Seq[File] = {
    import scala.collection.mutable.ArrayBuffer

    val generatedFiles = ArrayBuffer.empty[File]
    val pkgName = q"io.github.ablearthy.tl.functions"

    Files.createDirectories(basePath)

    val imports = List(
      q"import io.github.ablearthy.tl.aliases._",
      q"import io.github.ablearthy.tl.codecs._",
      q"import io.github.ablearthy.tl.types._"
    )

    val sources = defs.map(x => gen.generateFunction(x))
    sources.foreach { funit =>
      val fPath = basePath.resolve(s"${funit.fname}.scala")
      generatedFiles += fPath.toFile
      val source: Source = source"package $pkgName { ..$imports; ..${funit.source} }"
      Files.writeString(fPath, source.toString)
    }

    val encoders = sources.flatMap(_.encoders).toList

    val objEnc = q"object encoders { ..$encoders }"

    val implicitImports = List(
      q"import io.github.ablearthy.tl.codecs._",
      q"import io.circe.Encoder",
      q"import io.circe.generic.semiauto._",
      q"import io.github.ablearthy.tl.types.encoders._"
    )
    val implicitSource = source"package $pkgName { ..$implicitImports; $objEnc }"
    val implicitsPath = basePath.resolve("implicits.scala")
    Files.writeString(implicitsPath, implicitSource.toString)

    generatedFiles += implicitsPath.toFile
    generatedFiles
  }

}
