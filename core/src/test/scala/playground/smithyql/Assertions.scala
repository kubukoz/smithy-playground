package playground.smithyql

import cats.Id
import cats.implicits._
import weaver._

object Assertions extends Expectations.Helpers {

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
          ) = n.fold(struct = _ => "struct", int = _ => "int", string = _ => "string")

          ensureEqual(tpe(a), tpe(b))(ctx)
      }

    def compareStruct(lhs: Struct[Id], rhs: Struct[Id])(ctx: List[String]): Expectations =
      ensureEqual(
        lhs.fields.size,
        lhs.fields.keySet.map(_.text).size,
      )("key-size-lhs" :: ctx) &&
        ensureEqual(
          rhs.fields.size,
          rhs.fields.keySet.map(_.text).size,
        )("key-size-rhs" :: ctx) &&
        ensureEqual(lhs.fields.keySet.map(_.text), rhs.fields.keySet.map(_.text))(
          "keySet" :: ctx
        ) &&
        (
          (lhs.fields: Map[Struct.Key, InputNode[Id]]),
          rhs.fields: Map[Struct.Key, InputNode[Id]],
        ).tupled
          .map { case (k, (lhs, rhs)) => compareNode(lhs, rhs)(k.text :: ctx) }
          .toList
          .combineAll

    ensureEqual(lhs.operationName, rhs.operationName)("op_name" :: Nil) &&
    compareStruct(lhs.input, rhs.input)("query" :: Nil)
  }

}
