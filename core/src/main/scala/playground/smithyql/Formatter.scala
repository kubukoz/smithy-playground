package playground.smithyql

import org.typelevel.paiges.Doc
import cats.implicits._

object Formatter {

  def format(
    q: Query[WithSource],
    w: Int,
  ): String = writeQuery(q).renderTrim(w)

  def writeAst(ast: InputNode[WithSource]): Doc =
    ast match {
      case s @ Struct(_)     => renderStruct(s)
      case IntLiteral(i)     => Doc.text(i.toString())
      case BooleanLiteral(b) => Doc.text(b.toString())
      case StringLiteral(s)  => Doc.text(renderStringLiteral(s))
      case l @ Listed(_)     => renderSequence(l)
    }

  def renderUseClause(
    clause: UseClause
  ): Doc = Doc
    .text("use")
    .space("service")
    .space(renderIdent(clause.identifier))

  def renderIdent(ident: QualifiedIdentifier): Doc =
    Doc.intercalate(
      Doc.char('.'),
      ident.segments.map(Doc.text(_)).toList,
    ) + Doc.char('#') + Doc
      .text(
        ident.selection
      )

  def renderKey(k: WithSource[Struct.Key]): Doc =
    comments(k.commentsLeft) +
      Doc.text(k.value.text) +
      Doc.space +
      comments(k.commentsRight)

  def renderValue(v: WithSource[InputNode[WithSource]]): Doc = {
    val maybeGrouped: Doc => Doc =
      if (v.value.kind == NodeKind.Struct)
        identity
      else
        _.nested(2).grouped

    maybeGrouped {
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
          else
            {
              val sep =
                if (v.value.kind == NodeKind.Struct)
                  Doc.hardLine
                else
                  Doc.space

              sep
            } +
              comments(v.commentsRight)
        }
    }
  }

  def renderField(k: WithSource[Struct.Key], v: WithSource[InputNode[WithSource]]): Doc =
    renderKey(k) +
      Doc.char('=') +
      renderValue(v)

  def renderFields[T](fields: List[T])(renderField: T => Doc): Doc =
    Doc
      .intercalate(
        // Force newlines between fields
        Doc.hardLine,
        fields
          .map(renderField),
      )
      .aligned + {
      if (fields.isEmpty)
        Doc.empty
      else
        Doc.space
    }

  def renderBracketed[T](
    fields: WithSource[List[T]]
  )(
    before: Doc,
    after: Doc,
  )(
    renderField: T => Doc
  ): Doc =
    before + Doc.hardLine + {
      comments(fields.commentsLeft) +
        renderFields(fields.value)(renderField(_) + Doc.comma) +
        comments(fields.commentsRight)
    }
      .indent(2) +
      Doc.hardLine +
      after

  def renderStruct(struct: Struct[WithSource]): Doc =
    renderBracketed(struct.fields.map(_.value))(Doc.char('{'), Doc.char('}'))(renderField.tupled)

  def renderSequence(seq: Listed[WithSource]): Doc =
    renderBracketed(seq.values)(Doc.char('['), Doc.char(']'))(renderValue(_))

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

  def writeQuery(q: Query[WithSource]): Doc =
    // note: commenting a clause seems to make it disappear on formatting
    q.useClause
      .fold(Doc.empty)(a =>
        comments(a.commentsLeft) +
          renderUseClause(a.value) +
          Doc.hardLine +
          comments(a.commentsRight) + Doc.hardLine
      ) +
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

}
