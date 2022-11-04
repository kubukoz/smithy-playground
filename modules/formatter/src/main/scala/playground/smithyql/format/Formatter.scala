package playground.smithyql.format

import cats.implicits._
import org.typelevel.paiges.Doc
import org.typelevel.paiges.instances._
import playground.smithyql._

trait Formatter[-Alg[_[_]]] {
  def format(repr: Alg[WithSource], width: Int): String
}

object Formatter {
  def apply[Alg[_[_]]](implicit F: Formatter[Alg]): Formatter[Alg] = F

  implicit val fileFormatter: Formatter[SourceFile] = writeDoc
  implicit val queryFormatter: Formatter[Query] = writeDoc
  implicit val structFormatter: Formatter[Struct] = writeDoc
  implicit val listedFormatter: Formatter[Listed] = writeDoc

  def writeDoc: Formatter[AST] = FormattingVisitor(_).renderTrim(_)

}

private[format] object FormattingVisitor extends ASTVisitor[WithSource, Doc] { visit =>

  override def sourceFile(
    prelude: Prelude[WithSource],
    statements: WithSource[List[Statement[WithSource]]],
  ): Doc = Doc.stack(
    // Doc.hardLine.repeat(2),
    List(
      visit(prelude),
      comments(statements.commentsLeft) +
        statements.value.map(visit(_)).combineAll +
        comments(statements.commentsRight),
    ).filterNot(_.isEmpty)
  )

  override def prelude(useClause: Option[WithSource[UseClause[WithSource]]]): Doc =
    useClause match {
      case None => Doc.empty
      case Some(useClause) =>
        comments(useClause.commentsLeft) +
          visit(useClause.value) +
          Doc.hardLine +
          comments(useClause.commentsRight)
    }

  override def operationName(text: String): Doc = Doc.text(text)

  override def useClause(identifier: WithSource[QualifiedIdentifier]): Doc =
    // comments in clause are not allowed so we can ignore them when printing
    Doc
      .text("use")
      .space("service")
      .space(writeIdent(identifier.value))

  override def runQuery(query: WithSource[Query[WithSource]]): Doc =
    comments(query.commentsLeft) +
      visit(query.value) +
      comments(query.commentsRight)

  override def struct(fields: WithSource[Struct.Fields[WithSource]]): Doc =
    writeBracketed(fields.map(_.value))(Doc.char('{'), Doc.char('}'))(writeField)

  override def listed(values: WithSource[List[WithSource[InputNode[WithSource]]]]): Doc =
    writeBracketed(values)(Doc.char('['), Doc.char(']'))(writeValue(_))

  override def intLiteral(value: String): Doc = Doc.text(value)
  override def stringLiteral(value: String): Doc = Doc.text(writeStringLiteral(value))
  override def booleanLiteral(value: Boolean): Doc = Doc.text(value.show)
  override val nullLiteral: Doc = Doc.text("null")

  private def writeKey(k: WithSource[Identifier]): Doc =
    comments(k.commentsLeft) +
      Doc.text(k.value.text) +
      comments(k.commentsRight)

  private def writeField(binding: Binding[WithSource]): Doc = {
    val k = binding.identifier
    val v = binding.value

    writeKey(k) +
      Doc.char(':') +
      writeValue(v)
  }

  private def writeValue(v: WithSource[InputNode[WithSource]]): Doc = {
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

          sepBefore + visit(v.value)
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

  private def writeFields[T](fields: List[T])(renderField: T => Doc): Doc =
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

  private def writeBracketed[T](
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

  private def writeIdent(ident: QualifiedIdentifier): Doc = Doc.text(ident.render)

  private def writeStringLiteral(s: String) = "\"" + s + "\""

  private def comments(lines: List[Comment]): Doc = {
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
          lines.foldMap(lineComment(_) + Doc.hardLine)
    }
  }

  override def queryOperationName(
    identifier: Option[WithSource[QualifiedIdentifier]],
    operationName: WithSource[OperationName[WithSource]],
  ): Doc =
    // Comments inside this whole node are not allowed, so we ignore them
    identifier
      .map(_.value)
      .fold(Doc.empty)(writeIdent(_) + Doc.char('.')) +
      visit(operationName.value)

  override def query(
    useClause: WithSource[Option[UseClause[WithSource]]],
    operationName: WithSource[QueryOperationName[WithSource]],
    input: WithSource[Struct[WithSource]],
  ): Doc = {
    val opNamePart =
      comments(operationName.commentsLeft) +
        visit(operationName.value) +
        Doc.space +
        comments(operationName.commentsRight)

    val inputPart =
      comments(input.commentsLeft) +
        visit(input.value) + {
          if (input.commentsRight.isEmpty)
            Doc.empty
          else
            Doc.hardLine
        } +
        comments(input.commentsRight) + {
          if (input.commentsRight.isEmpty)
            Doc.hardLine
          else
            Doc.empty
        }

    opNamePart +
      inputPart
  }

}
