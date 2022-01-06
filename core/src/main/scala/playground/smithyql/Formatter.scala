package playground.smithyql

import org.typelevel.paiges.Doc

object Formatter {

  import AST.high._

  def writeAst(ast: InputNode[WithSource]): Doc =
    ast match {
      case Struct(fields) =>
        comments(fields.commentsLeft) +
          Doc.char('{') + Doc.hardLine + {
            comments(fields.value.commentsLeft) +
              Doc
                .intercalate(
                  // Force newlines between fields
                  Doc.hardLine,
                  fields
                    .value
                    .value
                    .toList
                    .map { case (k, v) =>
                      comments(k.commentsLeft) +
                        Doc.text(k.value) +
                        Doc.space +
                        comments(k.commentsRight) +
                        Doc.char('=') + {
                          v match {
                            case Struct(_) => Doc.space + writeAst(v)
                            case _         => (Doc.lineOrSpace + writeAst(v)).nested(2).grouped
                          }
                        } +
                        Doc.comma
                    },
                )
                .aligned + {
                if (fields.value.value.isEmpty)
                  Doc.empty
                else
                  Doc.space
              } + comments(fields.value.commentsRight)
          }.indent(2) + Doc.hardLine + Doc.char('}') + {

            val comms = comments(fields.commentsRight)
            if (comms.isEmpty)
              comms
            else
              Doc.hardLine + comms
          }

      case IntLiteral(i) => Doc.text(i.value.toString())
      case StringLiteral(s) =>
        comments(s.commentsLeft) +
          // todo: this can split multiline strings. wat do?
          Doc.char('\"') + Doc.text(s.value) + Doc.char('\"') + Doc.space +
          comments(s.commentsRight)
    }

  def comments(lines: List[Comment]): Doc = {
    def ensureLeadingSpace(s: String): String =
      if (s.startsWith(" "))
        s
      else
        " " + s

    def lineComment(s: Comment) = Doc.text("//" + ensureLeadingSpace(s.text))

    lines match {
      case Nil => Doc.empty
      case one :: Nil =>
        Doc.lineOrEmpty +
          lineComment(one) +
          Doc.hardLine
      case _ =>
        Doc.hardLine +
          Doc.cat(
            lines.map(lineComment(_) + Doc.hardLine).toList
          )
    }
  }

  def format(
    q: Query[WithSource],
    w: Int,
  ): String =
    (
      comments(q.operationName.commentsLeft) +
        Doc.text(q.operationName.value) +
        Doc.space +
        comments(q.operationName.commentsRight) +
        writeAst(q.input) +
        Doc.hardLine
    )
      .renderTrim(w)

}
