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

  sealed trait CommentPosition

  object CommentPosition {
    case object Before extends CommentPosition
    case object After extends CommentPosition
  }

  private def printWithComments[A](
    ast: WithSource[A]
  )(
    printA: A => Doc
  ) =
    comments(ast.commentsLeft, position = CommentPosition.Before) +
      printA(ast.value) +
      comments(ast.commentsRight, position = CommentPosition.After)

  private def printGeneric[A <: AST[WithSource]](ast: WithSource[A]) = printWithComments(ast)(visit)

  override def sourceFile(
    prelude: Prelude[WithSource],
    statements: WithSource[List[Statement[WithSource]]],
  ): Doc =
    Doc.stack(
      List(
        visit(prelude),
        printWithComments(statements)(_.foldMap(visit)),
      ).filterNot(_.isEmpty)
    )
    // files end with a newline character. They just do.
      + Doc.hardLine

  override def prelude(useClause: Option[WithSource[UseClause[WithSource]]]): Doc =
    useClause match {
      case None            => Doc.empty
      case Some(useClause) => printGeneric(useClause)
    }

  override def operationName(text: String): Doc = Doc.text(text)

  override def useClause(identifier: WithSource[QualifiedIdentifier]): Doc =
    // comments in clause are not allowed so we can ignore them when printing
    Doc
      .text("use")
      .space("service")
      .space(writeIdent(identifier.value))

  override def runQuery(query: WithSource[Query[WithSource]]): Doc = printGeneric(query)

  override def struct(fields: WithSource[Struct.Fields[WithSource]]): Doc =
    writeBracketed(fields.map(_.value))(Doc.char('{'), Doc.char('}'))(writeField)

  override def listed(values: WithSource[List[WithSource[InputNode[WithSource]]]]): Doc =
    writeBracketed(values)(Doc.char('['), Doc.char(']'))(writeValue(_))

  override def intLiteral(value: String): Doc = Doc.text(value)
  override def stringLiteral(value: String): Doc = Doc.text(writeStringLiteral(value))
  override def booleanLiteral(value: Boolean): Doc = Doc.text(value.show)
  override val nullLiteral: Doc = Doc.text("null")

  private def writeKey(k: WithSource[Identifier]): Doc = printWithComments(k)(v => Doc.text(v.text))

  private def writeField(binding: Binding[WithSource]): Doc = {
    val k = binding.identifier
    val v = binding.value

    val keyDoc = {
      val base = writeKey(k)

      if (k.commentsRight.nonEmpty)
        base + Doc.hardLine
      else
        base
    }

    val valueDoc = {
      val base = writeValue(v)

      if (v.commentsRight.nonEmpty)
        base + Doc.hardLine
      else
        base
    }

    keyDoc +
      Doc.str(": ") +
      valueDoc
  }

  private def writeValue(v: WithSource[InputNode[WithSource]]): Doc = {
    val maybeGrouped: Doc => Doc =
      if (v.value.kind == NodeKind.Struct || v.value.kind == NodeKind.Listed)
        identity
      else {
        _.nested(2).grouped
      }

    maybeGrouped {
      printGeneric(v)
      // {
      //   if (v.commentsLeft.nonEmpty)
      //     Doc.space
      //   else
      //     Doc.empty
      // } +
      //   comments(v.commentsLeft) + {
      //     val sepBefore =
      //       if (v.commentsLeft.nonEmpty)
      //         Doc.empty // hard line included in comment renderer
      //       else if (v.value.kind == NodeKind.Struct)
      //         Doc.space
      //       else
      //         Doc.lineOrSpace

      //     sepBefore + visit(v.value)
      //   } + {
      //     if (v.commentsRight.isEmpty)
      //       Doc.empty
      //     else
      //       {
      //         val sep =
      //           if (v.value.kind == NodeKind.Struct)
      //             Doc.hardLine
      //           else
      //             Doc.space

      //         sep
      //       } +
      //         comments(v.commentsRight)
      //   }
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
    before + Doc.hardLine +
      printWithComments(fields)(writeFields(_)(renderField(_) + Doc.comma))
        .indent(2) +
      Doc.hardLine +
      after

  private def writeIdent(ident: QualifiedIdentifier): Doc = Doc.text(ident.render)

  private def writeStringLiteral(s: String) = "\"" + s + "\""

  private def comments(lines: List[Comment], position: CommentPosition): Doc = {
    val internalString = Doc.intercalate(Doc.hardLine, lines.map(lineComment(_)))

    // TODO: the below should actually be true. this shouldn't be here
    // General note: spacing around the comments is the responsibility of the parent node.
    position match {
      case _ if lines.isEmpty => Doc.empty

      case CommentPosition.Before =>
        // a comment before anything MUST have a trailing line break
        internalString + Doc.hardLine

      case CommentPosition.After if lines.lengthIs == 1 =>
        // one line: we add a space before the comment
        Doc.lineOrSpace + internalString

      case CommentPosition.After =>
        // more lines: we force a hardline before the comments
        Doc.hardLine + internalString
    }
  }

  private def lineComment(s: Comment) = {
    def ensureLeadingSpace(s: String): String =
      if (s.startsWith(" "))
        s
      else
        " " + s
    Doc.text("//" + ensureLeadingSpace(s.text))
  }

  override def queryOperationName(
    identifier: Option[WithSource[QualifiedIdentifier]],
    operationName: WithSource[OperationName[WithSource]],
  ): Doc =
    // Comments inside this whole node are not allowed, but we use this anyway
    identifier.foldMap(printWithComments(_)(writeIdent(_) + Doc.char('.'))) +
      visit(operationName.value)

  override def query(
    useClause: WithSource[Option[UseClause[WithSource]]],
    operationName: WithSource[QueryOperationName[WithSource]],
    input: WithSource[Struct[WithSource]],
  ): Doc = printGeneric(operationName).space(printGeneric(input))

}
