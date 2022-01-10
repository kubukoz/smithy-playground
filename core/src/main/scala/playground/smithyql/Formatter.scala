package playground.smithyql

import org.typelevel.paiges.Doc

object Formatter {

  def writeAst(ast: InputNode[WithSource]): Doc =
    ast match {
      case Struct(fields) =>
        Doc.char('{') + Doc.hardLine + {
          comments(fields.commentsLeft) +
            Doc
              .intercalate(
                // Force newlines between fields
                Doc.hardLine,
                fields
                  .value
                  .toList
                  .sortBy(_._1.value.text)
                  .map { case (k, v) =>
                    val maybeGrouped: Doc => Doc =
                      if (v.value.kind == NodeKind.Struct)
                        identity
                      else
                        _.nested(2).grouped

                    comments(k.commentsLeft) +
                      Doc.text(k.value.text) +
                      Doc.space +
                      comments(k.commentsRight) +
                      Doc.char('=') + maybeGrouped {
                        {
                          if (v.commentsLeft.nonEmpty)
                            Doc.space
                          else
                            Doc.empty
                        } +
                          comments(v.commentsLeft) + {
                            val sepBefore =
                              if (v.commentsLeft.nonEmpty)
                                Doc.empty // hard line included in comment renderer
                              else if (v.value.kind == NodeKind.Struct)
                                Doc.space
                              else
                                Doc.lineOrSpace

                            sepBefore + writeAst(v.value)
                          } + {
                            if (v.commentsRight.isEmpty)
                              Doc.empty
                            else {
                              val sep =
                                if (v.value.kind == NodeKind.Struct)
                                  Doc.hardLine
                                else
                                  Doc.space

                              sep
                            } +
                              comments(v.commentsRight)
                          }
                      } +
                      Doc.comma
                  },
              )
              .aligned + {
              if (fields.value.isEmpty)
                Doc.empty
              else
                Doc.space
            } + comments(fields.commentsRight)
        }
          .indent(2) +
          Doc.hardLine +
          Doc.char('}')

      case IntLiteral(i)    => Doc.text(i.toString())
      case StringLiteral(s) => Doc.text(renderStringLiteral(s))
    }

  def renderStringLiteral(s: String) = "\"" + s + "\""

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
        Doc.text(q.operationName.value.text) +
        Doc.space +
        comments(q.operationName.commentsRight) +
        comments(q.input.commentsLeft) +
        writeAst(q.input.value) + {
          if (q.input.commentsRight.isEmpty)
            Doc.empty
          else
            Doc.hardLine
        } +
        comments(q.input.commentsRight) + {
          if (q.input.commentsRight.isEmpty)
            Doc.hardLine
          else
            Doc.empty
        }
    ).renderTrim(w)

}
