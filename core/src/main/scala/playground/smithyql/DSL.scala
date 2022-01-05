package playground.smithyql

import playground.smithyql.AST.high._
import cats.Id

object DSL {

  implicit class StringDSLOps(val s: String) extends AnyVal {

    def call(
      args: (String, InputNode[Id])*
    ): Query[Id] = Query[Id](s, Struct[Id](args.toMap))

  }

  def struct(args: (String, InputNode[Id])*): Struct[Id] = Struct[Id](args.toMap)

  implicit def stringToAST(s: String): StringLiteral[Id] = StringLiteral[Id](s)
  implicit def intToAST(i: Int): IntLiteral[Id] = IntLiteral[Id](i)

}
