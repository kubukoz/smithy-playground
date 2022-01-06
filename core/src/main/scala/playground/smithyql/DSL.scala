package playground.smithyql

import cats.Id
import cats.implicits._

object DSL {

  implicit class StringDSLOps(val s: String) extends AnyVal {

    def call(
      args: (String, InputNode[Id])*
    ): Query[Id] = Query[Id](OperationName(s), struct(args: _*))

  }

  def struct(
    args: (String, InputNode[Id])*
  ): Struct[Id] = Struct[Id](args.map(_.leftMap(Struct.Key(_))).toMap)

  implicit def stringToAST(s: String): StringLiteral[Id] = StringLiteral[Id](s)
  implicit def intToAST(i: Int): IntLiteral[Id] = IntLiteral[Id](i)

}
