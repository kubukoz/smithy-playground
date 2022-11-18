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

  val writeDoc: Formatter[AST] = FormattingVisitor(_).renderTrim(_)

  implicit val fileFormatter: Formatter[SourceFile] = writeDoc
  implicit val queryFormatter: Formatter[Query] = writeDoc
  implicit val useClauseFormatter: Formatter[UseClause] = writeDoc
  implicit val preludeFormatter: Formatter[Prelude] = writeDoc
  implicit val qonFormatter: Formatter[QueryOperationName] = writeDoc
  implicit val inputNodeFormatter: Formatter[InputNode] = writeDoc
  implicit val structFormatter: Formatter[Struct] = writeDoc
  implicit val listedFormatter: Formatter[Listed] = writeDoc

  def writeIdentifier(
    ident: QualifiedIdentifier,
    width: Int,
  ) = FormattingVisitor.writeIdent(ident).renderTrim(width)

}

private[format] object FormattingVisitor extends ASTVisitor[WithSource, Doc] { visit =>

  private def printWithComments[A](
    ast: WithSource[A]
  )(
    printA: A => Doc
  ) = {
    val commentsLHS = printComments(ast.commentsLeft)
    val commentsRHS = printComments(ast.commentsRight)

    val internal = printA(ast.value)

    val commentsLHSSep =
      if (internal.nonEmpty && commentsLHS.nonEmpty)
        Doc.hardLine
      else
        Doc.empty

    val commentsRHSSep =
      ast.commentsRight.length match {
        case 0 => Doc.empty
        case 1 => Doc.lineOrSpace
        case _ => Doc.hardLine
      }

    commentsLHS +
      commentsLHSSep +
      internal +
      commentsRHSSep +
      commentsRHS
    /*
       case CommentPosition.After if lines.lengthIs == 1 =>
        // one line: we add a space before the comment
        Doc.lineOrSpace + internalString

      case CommentPosition.After =>
        // more lines: we force a hardline before the comments
        Doc.hardLine + internalString
    }
     */
  }

  private def printGeneric(ast: WithSource[AST[WithSource]]) = printWithComments(ast)(visit)

  override def sourceFile(
    prelude: Prelude[WithSource],
    statements: WithSource[List[Statement[WithSource]]],
  ): Doc = List(
    visit(prelude),
    printWithComments(statements)(_.map(visit).intercalate(Doc.hardLine.repeat(2))),
  ).filterNot(_.isEmpty).intercalate(Doc.hardLine.repeat(2))

  override def prelude(
    useClauses: List[WithSource[UseClause[WithSource]]]
  ): Doc = useClauses.map(printGeneric).intercalate(Doc.hardLine)

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

  private def forceLineAfterTrailingComments[A](
    printer: WithSource[A] => Doc
  ): WithSource[A] => Doc =
    v =>
      if (v.commentsRight.nonEmpty)
        printer(v) + Doc.hardLine
      else
        printer(v)

  override def listed(values: WithSource[List[WithSource[InputNode[WithSource]]]]): Doc =
    writeBracketed(values)(Doc.char('['), Doc.char(']')) {
      forceLineAfterTrailingComments(printGeneric)
    }

  override def intLiteral(value: String): Doc = Doc.text(value)
  override def stringLiteral(value: String): Doc = Doc.text(writeStringLiteral(value))
  override def booleanLiteral(value: Boolean): Doc = Doc.text(value.show)
  override val nullLiteral: Doc = Doc.text("null")

  private def writeKey(k: WithSource[Identifier]): Doc = printWithComments(k)(v => Doc.text(v.text))

  private def writeField(binding: Binding[WithSource]): Doc = {
    val k = binding.identifier
    val v = binding.value

    forceLineAfterTrailingComments(writeKey)(k) +
      Doc.str(": ") +
      forceLineAfterTrailingComments(writeValue)(v)
  }

  private def writeValue(v: WithSource[InputNode[WithSource]]): Doc =
    v.value.kind match {
      // Structs and sequences introduce their own nesting, so we don't add it here.
      // however, if such a node occurs that has leading comments,
      // these comments will already force a hard line (see `printComments`).
      // That warrants an extra indent on such a node, so we fall through to the other case of this match.
      case NodeKind.Struct | NodeKind.Listed if v.commentsLeft.isEmpty => printGeneric(v)
      case _ => printGeneric(v).nested(2).grouped
    }

  private def writeFields[T](fields: List[T])(renderField: T => Doc): Doc =
    // Force newlines between fields
    fields.map(renderField).intercalate(Doc.hardLine)

  private def writeBracketed[T](
    items: WithSource[List[T]]
  )(
    before: Doc,
    after: Doc,
  )(
    renderItem: T => Doc
  ): Doc =
    before + Doc.hardLine +
      printWithComments(items)(writeFields(_)(renderItem(_) + Doc.comma))
        .indent(2) +
      Doc.hardLine +
      after

  def writeIdent(ident: QualifiedIdentifier): Doc = Doc.text(ident.render)

  private def writeStringLiteral(s: String) = "\"" + s + "\""

  private def printComments(
    lines: List[Comment]
  ): Doc = lines.map(lineComment(_)).intercalate(Doc.hardLine)

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
    operationName: WithSource[QueryOperationName[WithSource]],
    input: WithSource[Struct[WithSource]],
  ): Doc = {
    val nameInputSeparator =
      if (operationName.commentsRight.isEmpty)
        Doc.text(" ")
      else
        // If there are comments on the RHS of the op name, we're guaranteed a line break
        // so a space is redundant (it would've been part of the comment, in fact).
        Doc.empty

    forceLineAfterTrailingComments[AST[WithSource]](
      printGeneric(_) + nameInputSeparator
    )(operationName) + printGeneric(input)
  }

}
