package playground.smithyql

import org.typelevel.paiges.Doc
import cats.implicits._

object Formatter {

  def format(
    q: Query[WithSource],
    w: Int,
  ): String = writeQuery(q).renderTrim(w)

  def writeAst(ast: AST[WithSource]): Doc =
    ast match {
      case qo: QueryOperationName[WithSource] =>
        // Comments inside this whole node are not allowed, so we ignore them
        qo.identifier
          .map(_.value)
          .fold(Doc.empty)(renderIdent(_) + Doc.char('.')) +
          writeAst(qo.operationName.value)

      case o: OperationName[WithSource] => renderOperationName(o)
      case q: Query[WithSource]         => writeQuery(q)
      case u: UseClause[WithSource]     => renderUseClause(u)
      case n: InputNode[WithSource]     => writeInputNode(n)
    }

  def writeInputNode(ast: InputNode[WithSource]): Doc =
    ast match {
      case s @ Struct(_)     => renderStruct(s)
      case IntLiteral(i)     => Doc.text(i.toString())
      case BooleanLiteral(b) => Doc.text(b.toString())
      case StringLiteral(s)  => Doc.text(renderStringLiteral(s))
      case l @ Listed(_)     => renderSequence(l)
      case NullLiteral()     => Doc.text("null")
    }

  def renderOperationName(o: OperationName[WithSource]): Doc = Doc.text(o.text)

  def renderUseClause(
    clause: UseClause[WithSource]
  ): Doc =
    // comments in clause are not allowed so we can ignore them when printing
    Doc
      .text("use")
      .space("service")
      .space(renderIdent(clause.identifier.value))

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

          sepBefore + writeInputNode(v.value)
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

  def writeQuery(q: Query[WithSource]): Doc = {

    val useClausePart =
      comments(q.useClause.commentsLeft) +
        q.useClause.value.fold(Doc.empty)(renderUseClause(_) + Doc.hardLine) +
        comments(q.useClause.commentsRight) + (if (q.useClause.value.isEmpty)
                                                 Doc.empty
                                               else
                                                 Doc.hardLine)

    val opNamePart =
      comments(q.operationName.commentsLeft) +
        writeAst(q.operationName.value) +
        Doc.space +
        comments(q.operationName.commentsRight)

    val inputPart =
      comments(q.input.commentsLeft) +
        writeInputNode(q.input.value) + {
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

    useClausePart +
      opNamePart +
      inputPart
  }

}
