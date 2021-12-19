package playground

import org.typelevel.paiges.Doc
import playground._

object Formatter {

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
  ): String = (Doc.text(q.operationName) + Doc.space + writeAst(q.input)).render(40)

}
