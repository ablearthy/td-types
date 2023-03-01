package io.github.ablearthy.tl.gen

object GenUtils {
  def translateConstructorName(namespace: Option[String], name: String): String = {
    namespace match {
      case Some(ns) => s"${ns.capitalize}__${name.capitalize}"
      case None     => name.capitalize
    }
  }

  /** @param namespace
    *   namespace
    * @param name
    *   ident name
    * @return
    *   constructor name in a normal form (i.e. [namespace.]name)
    */
  def showConstructorName(namespace: Option[String], name: String): String = {
    namespace match {
      case Some(ns) => s"${ns}.${name}"
      case None     => name
    }
  }
}
