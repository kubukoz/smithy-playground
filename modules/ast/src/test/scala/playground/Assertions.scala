package playground

import cats.Id
import cats.syntax.all.*
import com.softwaremill.diffx.Diff
import com.softwaremill.diffx.ShowConfig
import playground.smithyql.*
import weaver.Expectations
import weaver.SourceLocation

object Assertions extends Expectations.Helpers {

  def assertNoDiff[A: Diff](
    actual: A,
    expected: A,
  )(
    implicit loc: SourceLocation
  ): Expectations =
    Diff[A].apply(expected, actual) match {
      case d if d.isIdentical => success
      case d =>
        val conf = ShowConfig.dark
        val stringWithResets = d.show()(conf).linesWithSeparators.map(Console.RESET + _).mkString

        failure(
          s"Diff failed:\n${Console.RESET}(${conf.left("expected")}, ${conf.right("actual")})\n\n" + stringWithResets
        )
    }

  def compareQuery(
    lhs: Query[Id],
    rhs: Query[Id],
  ): Expectations = {
    def ensureEqual[A](
      lhs: A,
      rhs: A,
    )(
      ctx: List[String]
    ): Expectations =
      if (lhs != rhs)
        failure(s"(${ctx.reverse.mkString("root / ", " / ", "")}) Comparison failed: $lhs != $rhs")
      else
        success

    def compareNode(
      lhs: InputNode[Id],
      rhs: InputNode[Id],
    )(
      ctx: List[String]
    ): Expectations =
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

    def compareStruct(
      lhs: Struct[Id],
      rhs: Struct[Id],
    )(
      ctx: List[String]
    ): Expectations =
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
