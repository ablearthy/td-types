package io.github.ablearthy.tl

import sbt._

object GeneratorPlugin extends AutoPlugin {
    def generateTypes(tlFile: File, basePath: File): Seq[File] = Generator.generateTypes(tlFile, basePath)
}