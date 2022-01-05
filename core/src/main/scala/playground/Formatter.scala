package playground

import org.typelevel.paiges.Doc
import playground._
import cats.Id

object Formatter {
  import AST.high._

  def writeAst(a: AST[Id]): Doc =
    a match {
      case Struct(fields) =>
        Doc
          .intercalate(
            Doc.line,
            fields.toList.map { case (k, v) =>
              Doc.text(k) +
                Doc.space +
                Doc.char('=') + {
                  if (v.isInstanceOf[Struct[Id]])
                    Doc.space + writeAst(v)
                  else
                    (Doc.lineOrSpace + writeAst(v)).nested(2).grouped

                } +
                Doc.comma
            },
          )
          .bracketBy(Doc.char('{'), Doc.char('}'))
      case IntLiteral(i)    => Doc.text(i.toString)
      case StringLiteral(s) => Doc.char('\"') + Doc.text(s) + Doc.char('\"')
    }

  def format(
    q: Query[Id],
    w: Int,
  ): String = (Doc.text(q.operationName) + Doc.space + writeAst(q.input) + Doc.hardLine)
    .renderTrim(w)

}