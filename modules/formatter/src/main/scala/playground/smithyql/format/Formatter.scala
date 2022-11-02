package playground.smithyql.format

import cats.implicits._
import org.typelevel.paiges.Doc
import org.typelevel.paiges.instances._
import playground.smithyql._

trait Formatter[Alg[_[_]]] {
  def format(repr: Alg[WithSource], width: Int): String
}

object Formatter {
  def apply[Alg[_[_]]](implicit F: Formatter[Alg]): Formatter[Alg] = F

  def writeDoc[Alg[_[_]]](writer: Alg[WithSource] => Doc): Formatter[Alg] = writer(_).renderTrim(_)

  implicit val fileFormatter: Formatter[SourceFile] = writeDoc(writeFile)
  implicit val queryFormatter: Formatter[Query] = writeDoc(writeQuery)
  implicit val structFormatter: Formatter[Struct] = writeDoc(writeStruct)
  implicit val listedFormatter: Formatter[Listed] = writeDoc(writeSequence)

  val writeAst: AST[WithSource] => Doc = {
    case sf: SourceFile[WithSource]         => writeFile(sf)
    case stat: Statement[WithSource]        => writeStatement(stat)
    case p: Prelude[WithSource]             => writePrelude(p)
    case qo: QueryOperationName[WithSource] =>
      // Comments inside this whole node are not allowed, so we ignore them
      qo.identifier
        .map(_.value)
        .fold(Doc.empty)(writeIdent(_) + Doc.char('.')) +
        writeOperationName(qo.operationName.value)

    case o: OperationName[WithSource] => writeOperationName(o)
    case q: Query[WithSource]         => writeQuery(q)
    case u: UseClause[WithSource]     => writeUseClause(u)
    case n: InputNode[WithSource]     => writeInputNode(n)
  }

  def writeFile(file: SourceFile[WithSource]): Doc =
    writePrelude(file.prelude) +
      file.statements.map(writeStatement).combineAll

  def writePrelude(prelude: Prelude[WithSource]): Doc =
    prelude.useClause match {
      case None => Doc.empty
      case Some(useClause) =>
        comments(useClause.commentsLeft) +
          writeUseClause(useClause.value) +
          comments(useClause.commentsRight)
    }

  def writeStatement(stat: Statement[WithSource]): Doc = stat.fold(
    runQuery =
      rq =>
        comments(rq.query.commentsLeft) +
          writeQuery(rq.query.value) +
          comments(rq.query.commentsRight)
  )

  def writeInputNode(ast: InputNode[WithSource]): Doc = ast.fold(
    struct = writeStruct,
    string = s => Doc.text(writeStringLiteral(s.value)),
    int = i => Doc.text(i.value),
    listed = writeSequence,
    bool = b => Doc.text(b.value.toString()),
    nul = _ => Doc.text("null"),
  )

  val writeOperationName: OperationName[WithSource] => Doc = o => Doc.text(o.text)

  def writeUseClause(
    clause: UseClause[WithSource]
  ): Doc =
    // comments in clause are not allowed so we can ignore them when printing
    Doc
      .text("use")
      .space("service")
      .space(writeIdent(clause.identifier.value))

  def writeIdent(ident: QualifiedIdentifier): Doc = Doc.text(ident.render)

  def writeKey(k: WithSource[Identifier]): Doc =
    comments(k.commentsLeft) +
      Doc.text(k.value.text) +
      comments(k.commentsRight)

  def writeValue(v: WithSource[InputNode[WithSource]]): Doc = {
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

  def writeField(binding: Binding[WithSource]): Doc = {
    val k = binding.identifier
    val v = binding.value

    writeKey(k) +
      Doc.char(':') +
      writeValue(v)
  }

  def writeFields[T](fields: List[T])(renderField: T => Doc): Doc =
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

  def writeBracketed[T](
    fields: WithSource[List[T]]
  )(
    before: Doc,
    after: Doc,
  )(
    renderField: T => Doc
  ): Doc =
    before + Doc.hardLine + {
      comments(fields.commentsLeft) +
        writeFields(fields.value)(renderField(_) + Doc.comma) +
        comments(fields.commentsRight)
    }
      .indent(2) +
      Doc.hardLine +
      after

  def writeStruct(struct: Struct[WithSource]): Doc =
    writeBracketed(struct.fields.map(_.value))(Doc.char('{'), Doc.char('}'))(writeField)

  def writeSequence(seq: Listed[WithSource]): Doc =
    writeBracketed(seq.values)(Doc.char('['), Doc.char(']'))(writeValue(_))

  def writeStringLiteral(s: String) = "\"" + s + "\""

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

    opNamePart +
      inputPart
  }

}
