package playground

import playground.AST._
import cats.Id

object DSL {

  implicit class StringDSLOps(val s: String) extends AnyVal {

    def call(args: (String, AST)*): Query = AST.high.Query[Id](s, AST.high.Struct[Id](args.toMap))
  }

  def struct(args: (String, AST)*): Struct = AST.high.Struct[Id](args.toMap)

  implicit def stringToAST(s: String): AST = StringLiteral[Id](s)
  implicit def intToAST(i: Int): AST = IntLiteral[Id](i)

}
