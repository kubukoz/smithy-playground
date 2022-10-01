package playground

import cats.effect.IO
import com.softwaremill.diffx.Diff
import weaver.Expectations
import weaver.Log
import weaver.SourceLocation
import com.softwaremill.diffx.ShowConfig
import playground.smithyql._
import cats.Id
import cats.implicits._

object Assertions extends Expectations.Helpers {

  import com.softwaremill.diffx.generic.auto._
  implicit val diffSourceRange: Diff[SourceRange] = Diff.derivedDiff
  implicit val diffQueryWithSource: Diff[Query[WithSource]] = Diff.derivedDiff
  implicit val diffDocumentSymbol: Diff[DocumentSymbol] = Diff.derivedDiff
  implicit val diffCompletionItem: Diff[CompletionItem] = Diff.derivedDiff

  def assertNoDiff[A: Diff](
    actual: A,
    expected: A,
  )(
    implicit loc: SourceLocation,
    log: Log[IO],
  ): IO[Expectations] =
    Diff[A].apply(expected, actual) match {
      case d if d.isIdentical => IO.pure(success)
      case d                  => log.info(d.show()(ShowConfig.dark)).as(failure("Diff failed"))
    }

  def compareQuery(
    lhs: Query[Id],
    rhs: Query[Id],
  ): Expectations = {
    def ensureEqual[A](lhs: A, rhs: A)(ctx: List[String]): Expectations =
      if (lhs != rhs)
        failure(s"(${ctx.reverse.mkString("root / ", " / ", "")}) Comparison failed: $lhs != $rhs")
      else
        success

    def compareNode(lhs: InputNode[Id], rhs: InputNode[Id])(ctx: List[String]): Expectations =
      (lhs, rhs) match {
        case (IntLiteral(i), IntLiteral(i2))       => ensureEqual(i, i2)("int" :: ctx)
        case (StringLiteral(s), StringLiteral(s2)) => ensureEqual(s, s2)("string" :: ctx)
        case (s @ Struct(_), s2 @ Struct(_))       => compareStruct(s, s2)("struct" :: ctx)
        case (a, b) =>
          def tpe(
            n: InputNode[Id]
          ) = n.fold(
            struct = _ => "struct",
            int = _ => "int",
            string = _ => "string",
            listed = _ => "list",
            bool = _ => "bool",
            nul = _ => "null",
          )

          ensureEqual(tpe(a), tpe(b))(ctx)
      }

    def compareStruct(lhs: Struct[Id], rhs: Struct[Id])(ctx: List[String]): Expectations =
      ensureEqual(
        lhs.fields.size,
        lhs.fields.keySet(identity).size,
      )("key-size-lhs" :: ctx) &&
        ensureEqual(
          rhs.fields.size,
          rhs.fields.keySet(identity).size,
        )("key-size-rhs" :: ctx) &&
        ensureEqual(lhs.fields.keySet(identity), rhs.fields.keySet(identity))(
          "keySet" :: ctx
        ) &&
        (
          rhs.fields.toMap: Map[Identifier, InputNode[Id]],
          lhs.fields.toMap: Map[Identifier, InputNode[Id]],
        ).tupled
          .map { case (k, (lhs, rhs)) => compareNode(lhs, rhs)(k.text :: ctx) }
          .toList
          .combineAll

    ensureEqual(lhs.operationName, rhs.operationName)("op_name" :: Nil) &&
    compareStruct(lhs.input, rhs.input)("query" :: Nil)
  }

}
