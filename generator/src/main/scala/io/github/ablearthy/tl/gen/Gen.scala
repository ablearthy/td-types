package io.github.ablearthy.tl.gen
import fastparse.{Parsed, parse}
import io.github.ablearthy.tl.parser.{Term => PTerm, _}

import scala.meta._

case class Gen(private val definitions: Vector[Definition]) {
  val groupedTypes: Map[IdentWithNs, Vector[Definition]] = definitions.groupBy { definition =>
    definition.resultType.ident
  }

  private val embeddedTypes =
    Map(
      "double" -> "Double",
      "string" -> "String",
      "vector" -> "Vector",
      "int32" -> "Int",
      "int53" -> "Long",
      "int64" -> "Long",
      "bytes" -> "Bytes"
    ).map { case (k, v) =>
      IdentWithNs(None, k) -> Type.Name(v)
    }

  // single (possibly, bare):
  // ctor -> ResultType
  // multiple:
  // ctor -> ResultType.CtorIdent

  val tlCtorToScalaType: Map[IdentWithNs, Type] = definitions.map { d =>
    val family = groupedTypes(d.resultType.ident)
    val ctor = IdentWithNs(d.constructorName.namespace, d.constructorName.ident)
    if (family.length == 1) {
      val ident = d.resultType.ident
      val name = GenUtils.translateConstructorName(ident.namespace, ident.ident)
      ctor -> Type.Name(name)
    } else {
      val parentIdent = d.resultType.ident
      val parentName = GenUtils.translateConstructorName(parentIdent.namespace, parentIdent.ident)
      val baseName = d.constructorName.ident.capitalize

      ctor -> Type.Select(Term.Name(parentName), Type.Name(baseName))
    }
  }.toMap ++ embeddedTypes

  val tlCtorToScalaClassName: Map[IdentWithNs, Type.Name] = tlCtorToScalaType.map { case (k, v) =>
    v match {
      case a: Type.Name   => k -> a
      case b: Type.Select => k -> b.name
    }
  }

  val tlTypeToScalaType: Map[IdentWithNs, Type.Name] = groupedTypes.keys.map { ns =>
    val name = GenUtils.translateConstructorName(ns)
    ns -> Type.Name(name)
  }.toMap ++ Map("Bool" -> "Bool").map { case (k, v) =>
    IdentWithNs(None, k) -> Type.Name(v)
  }

  def generateFunction(definition: Definition): FileUnit = {
    val ctorName = definition.constructorName.namespace
      .getOrElse("")
      .capitalize + definition.constructorName.ident.capitalize + "Params"
    val comments = parse(definition.comments.mkString("\n"), CommentParser.parser(_)) match {
      case Parsed.Success(value, index) => value.toVector
      case _                            => Vector.empty
    }
    val params = definition.args.zipWithIndex
      .flatMap { case (u, i) =>
        generateParam(u, i, comments)
      }
      .map { p =>
        p match {
          case param"$name: Option[$typ]" => param"$name: Option[$typ] = None"
          case _                          => p
        }
      }
      .toList
    val returnType = tlTypeToScalaType(definition.resultType.ident)
    val source = q"case class ${Type.Name(ctorName)}(..$params) extends TLFunction[$returnType]"
    val encoder = generateCodecsForFunction(definition)
    FileUnit(
      fname = ctorName,
      source = List(source),
      encoders = List(encoder),
      decoders = List.empty
    )
  }

  def generateSingleton(
      name: Type.Name,
      definition: Definition,
      maybeExtends: Option[Type.Name] = None
  ): Defn.Class = {

    val typeParams = getTypeParams(definition.resultType)
    val realTypeParameters = typeParams.map(x => tparam"$x")
    val comments = parse(definition.comments.mkString("\n"), CommentParser.parser(_)) match {
      case Parsed.Success(value, index) => value.toVector
      case _                            => Vector.empty
    }
    val params = definition.args.zipWithIndex.flatMap { case (u, i) =>
      generateParam(u, i, comments)
    }.toList

    maybeExtends match {
      case Some(ex) => {
        val t = typeParams match {
          case Nil => t"$ex"
          case _   => t"$ex[..$typeParams]"
        }
        val r = meta.Init(t, Name(""), Nil)
        q"case class $name[..$realTypeParameters](..$params) extends $r"
      }
      case None => q"case class $name[..$realTypeParameters](..$params)"
    }
  }

  case class FileUnit(fname: String, source: List[Defn], encoders: List[Defn], decoders: List[Defn])

  def generate(iwn: IdentWithNs): FileUnit = {
    val family = groupedTypes(iwn)

    if (family.length == 1) {
      val tlClassDefn = family(0)
      val ident =
        IdentWithNs(tlClassDefn.constructorName.namespace, tlClassDefn.constructorName.ident)
      val typeName = tlCtorToScalaClassName(ident)
      val classDefn = generateSingleton(typeName, family(0))

      val (enc, dec) = generateCodecsForCtor(tlClassDefn)

      FileUnit(typeName.toString, List(classDefn), List(enc), List(dec))
    } else {
      val mainType = tlTypeToScalaType(iwn)
      val typeParams = getTypeParams(family(0).resultType).map(x => tparam"$x")
      val mainTrait = q"sealed trait $mainType[..$typeParams]"
      val defs = family.map { d =>
        val ident = IdentWithNs(d.constructorName.namespace, d.constructorName.ident)
        val typeName = tlCtorToScalaClassName(ident)
        generateSingleton(typeName, d, Some(mainType))
      }.toList
      // println(mainTrait)
      // println(companion)

      val codecs = family.map(generateCodecsForCtor)

      val encoders = generateEncoderForSum(iwn) :: codecs.map(_._1).toList
      val decoders = generateDecoderForSum(iwn) :: codecs.map(_._2).toList
      FileUnit(
        mainType.toString,
        List(
          mainTrait,
          q"object ${Term.Name(mainType.toString)} { ..$defs }"
        ),
        encoders,
        decoders
      )
    }
  }

  def shouldBeNullable(comments: Vector[(String, String)], lit: String): Boolean = {
    val r =
      comments.find { case (u, _) => u == lit }.map { case (_, v) =>
        v.contains("may be null") || v.contains("pass null")
      }
    r match {
      case Some(value) => value
      case None        => false
    }
  }

  def generateParam(arg: Args, index: Int, comments: Vector[(String, String)]): List[Term.Param] =
    arg match {
      case CondArg(ident, cond, _, typeTerm) => {
        val lit = ident.getOrElse(s"anon__$index")
        val name = meta.Name(lit)
        val scalaType = transformPTerm(typeTerm)
        if (cond.isDefined || shouldBeNullable(comments, lit)) {
          List(param"$name: Option[$scalaType]")
        } else {
          List(param"$name: $scalaType")
        }
      }
      case ArrayArg(ident, multiplicity, args) => {
        val rest = Type.Tuple(args.map { case AnonymousArg(_, term) =>
          transformPTerm(term)
        }.toList)
        val name = meta.Name(ident.getOrElse(s"anon__$index"))
        List(param"$name: Vector[$rest]")
      }
      case BracketArg(idents, exclMark, typeTerm) => {
        val tpe = transformPTerm(typeTerm)
        idents.zipWithIndex
          .map { case (a, internalIndex) =>
            val lit = a.getOrElse(s"anon__${index}__$internalIndex")
            (meta.Name(lit), shouldBeNullable(comments, lit))
          }
          .map { case (name, maybeNullable) =>
            if (maybeNullable) {
              param"$name: Option[$tpe]"
            } else {
              param"$name: $tpe"
            }
          }
          .toList
      }
      case AnonymousArg(exclMark, typeTerm) => {
        val name = meta.Name(s"anon__$index")
        List(param"$name: ${transformPTerm(typeTerm)}")
      }
    }

  def getTypeParams(resultType: ResultType): List[Type.Name] = {
    resultType.terms.map { case SimpleTerm(_, SimpleTypeIdent(IdentWithNs(_, n))) =>
      meta.Type.Name(n.capitalize)
    }.toList
  }

  def transformPTerm(term: PTerm): Type = term match {
    case BracketTerm(bare, fst +: rest) => {
      val tpe = transformPTerm(fst)
      val cons = rest.map(transformPTerm).toList
      t"$tpe[..$cons]"
    }
    case SimpleTerm(bare, s) => transformTypeIdent(bare, s)
    case AngleBracketTerm(bare, typeIdent, rest) => {
      val tpe = transformTypeIdent(bare, typeIdent)
      val cons = rest.map(transformPTerm).toList
      t"$tpe[..$cons]"
    }
  }

  def transformTypeIdent(bare: Boolean, typeIdent: TypeIdent): Type = typeIdent match {
    case SimpleTypeIdent(iwn @ IdentWithNs(namespace, ident)) => {
      if (ident == "Vector") {
        Type.Name("Vector")
      } else if (ident(0).isLower) {
        tlCtorToScalaType(iwn)
      } else {
        tlTypeToScalaType(iwn)
      }
    }
    case HashTypeIdent => Type.Name("Int")
  }

  def generateCodecsForFunction(definition: Definition): Defn = {
    val rawName = GenUtils.showConstructorName(
      namespace = definition.constructorName.namespace,
      name = definition.constructorName.ident
    )

    val encoderType = definition.constructorName.namespace
      .getOrElse("")
      .capitalize + definition.constructorName.ident.capitalize + "Params"

    val encoderName = encoderType(0).toLower + encoderType.substring(1)

    val encoderAsObject = t"Encoder.AsObject[${Type.Name(encoderType)}]"
    val r = Pat.Var(Term.Name(encoderName + "Encoder"))
    val lit = Lit.String(rawName)
    q"lazy implicit val $r: $encoderAsObject = TDJsonEncoder.deriveProductEncoder($lit, deriveEncoder)"
  }

  def generateCodecsForCtor(definition: Definition): (Defn, Defn) = {
    val rawName = GenUtils.translateConstructorName(
      namespace = definition.constructorName.namespace,
      name = definition.constructorName.ident
    )
    val prefixName = GenUtils.lowercase(rawName)
    val scalaTypeName = tlCtorToScalaType(
      IdentWithNs(definition.constructorName.namespace, definition.constructorName.ident)
    )
    val ctorName = Lit.String(rawName(0).toLower + rawName.substring(1))

    val r = Pat.Var(Term.Name(s"${prefixName}Encoder"))
    val r1 = Pat.Var(Term.Name(s"${prefixName}Decoder"))

    val qEncoder =
      q"lazy implicit val $r: Encoder.AsObject[$scalaTypeName] = TDJsonEncoder.deriveProductEncoder($ctorName, deriveEncoder)"
    val qDecoder =
      q"lazy implicit val $r1: Decoder[${scalaTypeName}] = TDJsonDecoder.deriveProductDecoder($ctorName, deriveDecoder)"
    (qEncoder, qDecoder)
  }

  def generateDecoderForSum(iwn: IdentWithNs): Defn = {
    val family = groupedTypes(iwn)
    val mainBaseName = GenUtils.lowercase(GenUtils.translateConstructorName(iwn))

    val decoderName = Pat.Var(Term.Name(s"${mainBaseName}Decoder"))
    val decoderT = t"Decoder[${tlTypeToScalaType(iwn)}]"
    val defs = family.map { definition =>
      val rawName = GenUtils.translateConstructorName(
        namespace = definition.constructorName.namespace,
        name = definition.constructorName.ident
      )
      val prefixName = GenUtils.lowercase(rawName)
      val x = Term.Name(s"${prefixName}Decoder")
      q"$x.widen"
    }.toList
    q"lazy implicit val $decoderName: $decoderT = List[$decoderT](..$defs).reduceLeft(_ or _)"
  }

  def generateEncoderForSum(iwn: IdentWithNs): Defn = {
    val family = groupedTypes(iwn)
    val mainBaseName = GenUtils.lowercase(GenUtils.translateConstructorName(iwn))
    val encoderName = Pat.Var(Term.Name(s"${mainBaseName}Decoder"))
    val encoderT = t"Encoder.AsObject[${tlTypeToScalaType(iwn)}]"
    val defs = family.map { definition =>
      val ident =
        IdentWithNs(definition.constructorName.namespace, definition.constructorName.ident)
      val rawName = GenUtils.translateConstructorName(ident)
      val prefixName = GenUtils.lowercase(rawName)
      val x = Term.Name(s"${prefixName}Encoder")
      p"case a: ${tlCtorToScalaType(ident)} => $x.encodeObject(a)"
    }.toList
    q"lazy implicit val $encoderName: $encoderT = Encoder.AsObject.instance { ..case $defs }"
  }
}

// object Gen {
//   def main(args: Array[String]): Unit = {
//     val x = Term.Name("type_")
//     val params: List[Type.Param] = List.empty
//     println(q"case class X[..$params]()")
//   }
// }
