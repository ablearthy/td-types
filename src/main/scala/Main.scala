import fastparse.{Parsed, parse}
import io.circe._
import io.circe.syntax._
import io.github.ablearthy.tl.gen.Gen
import io.github.ablearthy.tl.parser.schemaParser
import shapeless.{Coproduct, Generic}

import java.io._
import scala.meta._

object Main {

  def main(args: Array[String]): Unit = {
    val td_api = getClass.getResourceAsStream("td_api.tl")
    val result =
      parse(
        td_api,
        schemaParser(_)
      )
    result match {
      case Parsed.Success(r, _) => {
        val gen = Gen(r.types)
        val defs = gen.types.flatMap { case (k, v) =>
          if (v.length == 1) {
            List(gen.generateSingleton(v(0)))
          } else {
            gen.generateSum(k, v)
          }
        }.toList

        val implicits = gen.types.flatMap { case (k, v) =>
          v.flatMap { d =>
            val t = gen.generateCodecsForProduct(d)
            List(t._1, t._2)
          }
        }.toList

        val sumImplicits = gen.types
          .filter { case (_, v) => v.length > 1 }
          .flatMap { case (k, v) =>
            List(gen.generateEncoderForSum(k, v), gen.generateDecoderForSum(k, v))
          }
          .toList

        val implicitsQ = q"object TDJsonImplicits { ..$implicits; ..$sumImplicits }"

        val source = source"..$defs; $implicitsQ"
        val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("test.txt")))
        writer.write(source.toString)
        writer.close
      }

      case failure: Parsed.Failure => println(s"Failure: $failure")
    }
  }
}
