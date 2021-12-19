package playground

object DSL {

  implicit class StringDSLOps(val s: String) extends AnyVal {

    def call(args: (String, AST)*): Query = Query(s, Struct(args.toMap))
  }

  def struct(args: (String, AST)*): Struct = Struct(args.toMap)

  implicit def stringToAST(s: String): AST = StringLiteral(s)
  implicit def intToAST(i: Int): AST = IntLiteral(i)

}
