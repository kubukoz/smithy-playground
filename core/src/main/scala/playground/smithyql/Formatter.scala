package playground.smithyql

import org.typelevel.paiges.Doc

object Formatter {
  import AST.high._

  def writeAst(ast: InputNode[WithSource]): Doc =
    ast match {
      case Struct(fields) =>
        Doc
          .intercalate(
            Doc.line,
            fields
              .value
              .toList
              .map { case (k, v) =>
                comments(k.commentsLeft) +
                  Doc.text(k.value) +
                  comments(k.commentsRight) +
                  Doc.space +
                  Doc.char('=') + {
                    v match {
                      case Struct(_) => Doc.space + writeAst(v)
                      case _         => (Doc.lineOrSpace + writeAst(v)).nested(2).grouped
                    }
                  } +
                  Doc.comma
              },
          )
          .bracketBy(comments(fields.commentsLeft), comments(fields.commentsRight))
          .bracketBy(Doc.char('{'), Doc.char('}'))

      case IntLiteral(i) => Doc.text(i.value.toString())
      case StringLiteral(s) =>
        comments(s.commentsLeft) +
          Doc.char('\"') + Doc.text(s.value) + Doc.char('\"') +
          comments(s.commentsRight)
    }

  def comment(text: String): Doc = Doc.text("//" + text) + Doc.hardLine
  def comments(commies: List[Comment]): Doc = Doc.cat(commies.map(comment.compose(_.text)))

  def format(
    q: WithSource[Query[WithSource]],
    w: Int,
  ): String =
    (
      comments(q.commentsLeft) +
        comments(q.value.operationName.commentsLeft) +
        Doc.text(q.value.operationName.value) +
        Doc.space +
        comments(q.value.operationName.commentsRight) +
        writeAst(q.value.input) +
        comments(q.commentsRight) +
        Doc.hardLine
    )
      .renderTrim(w)

}
