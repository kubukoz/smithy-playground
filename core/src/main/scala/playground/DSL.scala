package playground

import playground.AST.high._
import cats.Id

object DSL {

  implicit class StringDSLOps(val s: String) extends AnyVal {

    def call(
      args: (String, InputNode[Id])*
    ): Query[Id] = AST.high.Query[Id](s, AST.high.Struct[Id](args.toMap))

  }

  def struct(args: (String, InputNode[Id])*): Struct[Id] = AST.high.Struct[Id](args.toMap)

  implicit def stringToAST(s: String): StringLiteral[Id] = StringLiteral[Id](s)
  implicit def intToAST(i: Int): IntLiteral[Id] = IntLiteral[Id](i)

}
