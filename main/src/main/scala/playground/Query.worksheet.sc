import scala.util.control.NoStackTrace

import cats.parse.Parser0

import cats.parse.Rfc5234

import cats.parse.Numbers

import cats.parse.Parser

import cats.data.Kleisli

import cats.effect

import cats.effect.kernel.Resource
import smithy4s.Endpoint

import smithy4s.Service

import org.http4s.ember.client.EmberClientBuilder

import cats.data.NonEmptyList

import schematic.ByteArray

import cats.implicits._
import smithy4s.Document
import smithy4s.Hints
import schematic.Field
import java.util.UUID
import schematic.Alt
import smithy4s.Timestamp

import org.http4s.implicits._

import cats.effect.IO
import smithy4s.http4s.SimpleRestJsonBuilder
import com.disneystreaming.demo.smithy.DemoServiceGen

case class Query(
  operationName: String,
  input: Struct,
)

sealed trait AST extends Product with Serializable

case class Struct(
  fields: Map[String, AST]
) extends AST

case class IntLiteral(value: Int) extends AST
case class StringLiteral(value: String) extends AST

class QuerySchematic
  extends smithy4s.Schematic[AST => *]
  with schematic.struct.GenericAritySchematic[AST => *] {
  def short: AST => Short = ???

  def int: AST => Int = { case IntLiteral(i) => i }

  def long: AST => Long = ???

  def double: AST => Double = ???

  def float: AST => Float = ???

  def bigint: AST => BigInt = ???

  def bigdecimal: AST => BigDecimal = ???

  def string: AST => String = { case StringLiteral(s) => s }

  def boolean: AST => Boolean = ???

  def uuid: AST => UUID = ???

  def byte: AST => Byte = ???

  def bytes: AST => ByteArray = ???

  def unit: AST => Unit = ???

  def list[S](fs: AST => S): AST => List[S] = ???

  def set[S](fs: AST => S): AST => Set[S] = ???

  def vector[S](fs: AST => S): AST => Vector[S] = ???

  def map[K, V](fk: AST => K, fv: AST => V): AST => Map[K, V] = ???

  def genericStruct[S](
    fields: Vector[Field[AST => *, S, _]]
  )(
    const: Vector[Any] => S
  ): AST => S = { case Struct(asts) =>
    const {
      fields.map { field =>
        field.instance(asts(field.label))
      }
    }

  }

  def union[S](
    first: Alt[AST => *, S, _],
    rest: Vector[Alt[AST => *, S, _]],
  )(
    total: S => Alt.WithValue[AST => *, S, _]
  ): AST => S = {
    case Struct(defs) if defs.size == 1 =>
      def go[A](alt: Alt[AST => *, S, A]): AST => S = alt.instance.andThen(alt.inject)

      val (k, v) = defs.head
      val opts = NonEmptyList(first, rest.toList)
      val op = opts
        .find { e =>
          e.label == k
        }
        .getOrElse(
          throw new Exception(
            "wrong shape, this union requires one of: " + opts
              .map(_.label)
              .mkString_(", ")
          )
        )

      go(op)(v)

  }

  def enumeration[A](
    to: A => (String, Int),
    fromName: Map[String, A],
    fromOrdinal: Map[Int, A],
  ): AST => A = ???

  def suspend[A](f: => AST => A): AST => A = ???

  def bijection[A, B](f: AST => A, to: A => B, from: B => A): AST => B = f.andThen(to)

  def timestamp: AST => Timestamp = ???

  def withHints[A](fa: AST => A, hints: Hints): AST => A = fa // todo

  def document: AST => Document = ???

}

class Compiler[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
  service: Service[Alg, Op]
) {

  private val schem = new QuerySchematic

  // for quick lookup and decoding from AST
  private val endpoints: Map[String, AST => Op[_, _, _, _, _]] = {
    def go[I](
      endpoint: Endpoint[Op, I, _, _, _, _]
    ) = endpoint.input.compile(schem).andThen(endpoint.wrap)

    service
      .endpoints
      .groupByNel(_.name)
      .map(_.map(_.head).map(go(_)))
  }

  def compile(q: Query): Op[_, _, _, _, _] = endpoints(q.operationName)(q.input)

}

class Runner[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _], F[_]](
  service: Service[Alg, Op],
  client: Alg[smithy4s.GenLift[F]#λ],
) {

  private val exec = service.asTransformation(client)
  private val compiler = new Compiler(service)

  def run(q: Query) = exec(compiler.compile(q))
}

import cats.effect.unsafe.implicits._

def mkRunner[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
  service: Service[Alg, Op]
): Resource[IO, Runner[Alg, Op, IO]] = EmberClientBuilder
  .default[IO]
  .build
  .flatMap { c =>
    SimpleRestJsonBuilder(service).clientResource(c, uri"http://localhost:8082")
  }
  .map(new Runner(service, _))

import org.typelevel.paiges.Doc

def writeAst(a: AST): Doc =
  a match {
    case Struct(fields) =>
      Doc
        .intercalate(
          Doc.line,
          fields.toList.map { case (k, v) =>
            Doc.text(k) +
              Doc.space +
              Doc.char('=') +
              Doc.space +
              writeAst(v) +
              Doc.comma
          },
        )
        .bracketBy(Doc.char('{'), Doc.char('}'))
    case IntLiteral(i)    => Doc.text(i.toString)
    case StringLiteral(s) => Doc.text(s).tightBracketBy(Doc.char('"'), Doc.char('"'))
  }

def format(
  q: Query
) = (Doc.text(q.operationName) + Doc.space + writeAst(q.input)).render(40)

val parser: Parser[Query] = {
  import Parser._

  val symbol: Parser[String] = (Rfc5234.alpha ~ Parser.charsWhile0(_.isLetterOrDigit)).map {
    case (ch, s) => s.prepended(ch)
  }

  val optionalWhitespace: Parser0[Unit] = charsWhile0(_.isWhitespace).void

  lazy val ast: Parser[AST] = Parser.defer(intLiteral | stringLiteral | struct)

  lazy val intLiteral: Parser[IntLiteral] = Numbers.digits.map(_.toInt).map(IntLiteral)

  // todo: allow quotes inside
  lazy val stringLiteral: Parser[StringLiteral] = anyChar
    .repUntil0(char('\"'))
    .map(_.mkString)
    .with1
    .surroundedBy(char('"'))
    .map(StringLiteral)

  lazy val struct: Parser[Struct] = {
    val field: Parser[(String, AST)] =
      symbol ~ (
        optionalWhitespace *>
          char('=') *>
          optionalWhitespace *>
          ast
      )

    // field, then optional whitespace, then optional coma, then optionally more `fields`
    lazy val fields: Parser0[List[(String, AST)]] = Parser
      .defer {
        field ~ {
          optionalWhitespace *>
            (char(',') *>
              optionalWhitespace *>
              fields).?
        }
      }
      .?
      .map {
        _.fold(Nil: List[(String, AST)]) { case (a, b) => a :: b.getOrElse(Nil) }
      }

    fields
      .surroundedBy(optionalWhitespace)
      .map(_.toMap)
      .with1
      .between(
        char('{'),
        char('}'),
      )
      .map(Struct(_))
  }

  (symbol, optionalWhitespace.with1 *> struct)
    .mapN(Query.apply)
    .surroundedBy(optionalWhitespace)
}

def parse(s: String) =
  parser
    .parseAll(s)
    .leftMap { e =>
      val (valid, failed) = s.splitAt(
        e.failedAtOffset
      )

      throw new Exception(
        s"$valid<<<FAIL>>>$failed - expected ${e.expected.map(_.toString()).mkString_(", ")}"
      ) with NoStackTrace
    }
    .merge

val q = parse {
  """CreateHero { hero = { good = { howGood = 42 }, }, } """
}

println(format(q))

mkRunner(DemoServiceGen)
  .use {
    _.run {
      q
    }
  }
  .unsafeRunSync()
