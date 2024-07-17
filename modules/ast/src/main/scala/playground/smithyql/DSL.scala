package playground.smithyql

import cats.Id
import cats.syntax.all.*

object DSL {

  implicit class StringDSLOps(
    val s: String
  ) extends AnyVal {

    def call(
      args: (
        String,
        InputNode[Id],
      )*
    ): Query[Id] = Query[Id](
      operationName = QueryOperationName[Id](None, OperationName(s)),
      input = struct(args: _*),
    )

  }

  def struct(
    args: (
      String,
      InputNode[Id],
    )*
  ): Struct[Id] = Struct[Id](
    Struct.Fields.fromSeq[Id](args.map(_.leftMap(Identifier(_))).map(Binding.apply[Id].tupled))
  )

  implicit def stringToAST(
    s: String
  ): StringLiteral[Id] = StringLiteral[Id](s)

  implicit def intToAST(
    i: Int
  ): IntLiteral[Id] = IntLiteral[Id](i.toString)

  implicit def longToAST(
    i: Long
  ): IntLiteral[Id] = IntLiteral[Id](i.toString)

  implicit def floatToAST(
    i: Float
  ): IntLiteral[Id] = IntLiteral[Id](i.toString)

  implicit def doubleToAST(
    i: Double
  ): IntLiteral[Id] = IntLiteral[Id](i.toString)

  implicit def bigIntToAST(
    i: BigInt
  ): IntLiteral[Id] = IntLiteral[Id](i.toString)

  implicit def bigDecimalToAST(
    i: BigDecimal
  ): IntLiteral[Id] = IntLiteral[Id](i.toString)

  implicit def boolToAST(
    b: Boolean
  ): BooleanLiteral[Id] = BooleanLiteral[Id](b)

  implicit def listToAST[A](
    l: List[A]
  )(
    implicit ev: A => InputNode[Id]
  ): Listed[Id] = Listed[Id](l.map(ev))

}
