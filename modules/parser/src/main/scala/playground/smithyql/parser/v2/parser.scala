package playground.smithyql.parser.v2

import cats.Eval
import cats.data.NonEmptyList
import cats.implicits._
import playground.smithyql.parser.v2.scanner.Scanner
import playground.smithyql.parser.v2.scanner.Token
import playground.smithyql.parser.v2.scanner.TokenKind

case class GreenNode(
  kind: SyntaxKind,
  children: List[Either[GreenNode, Token]],
  errors: List[Error],
) {
  def toBuilder: GreenNode.GreenNodeBuilder = GreenNode.builder(kind).addAll(children)

  def allTokens: List[Token] = children.flatMap {
    _.fold(_.allTokens, _.some)
  }

  lazy val width: Int = children.foldMap(_.fold(_.width, _.text.length()))

  def print: String = {
    def go(
      depth: Int,
      self: GreenNode,
    ): String =
      "  " * depth +
        s"${self.kind}:${self.width}\n" +
        self
          .children
          .map {
            case Left(node)   => go(depth + 1, node)
            case Right(token) => "  " * (depth + 1) + token.text
          }
          .mkString("\n")

    go(0, this)
  }

}

object GreenNode {

  def builder(
    kind: SyntaxKind
  ) = new GreenNodeBuilder(kind)

  def error(
    token: Token
  ): GreenNode = builder(SyntaxKind.ERROR).addChild(token).build()

  class GreenNodeBuilder(
    kind: SyntaxKind
  ) {
    private var _children: Vector[Either[GreenNode, Token]] = Vector.empty
    private var _errors: List[Error] = Nil

    def addChild(
      child: GreenNode
    ): this.type = addChild(child.asLeft)

    def addChild(
      child: Token
    ): this.type = addChild(child.asRight)

    def addChild(
      child: Either[GreenNode, Token]
    ): this.type = {
      this._children :+= child
      this
    }

    def addAll(
      children: List[Either[GreenNode, Token]]
    ): this.type = {
      children.foreach(addChild)
      this
    }

    def addErrorToken(
      e: Token
    ): this.type = addChild(error(e))

    def addError(
      e: Error
    ): this.type = {
      this._errors ::= e
      this
    }

    def build(
    ): GreenNode = GreenNode(
      kind = kind,
      children = _children.toList,
      errors = _errors,
    )

  }

}

case class SyntaxNode(
  offset: Int,
  parent: Eval[Option[SyntaxNode]],
  green: Either[GreenNode, Token],
) {

//   // def cast[A](
//   //   implicit mirror: AstNodeMirror[A]
//   // ): Option[A] = mirror.cast(this)

  def width = green.fold(_.width, _.width)

  def range: (
    Int,
    Int,
  ) = (offset, offsetUntil)

  def offsetUntil: Int = offset + width

  def includes(
    index: Int
  ): Boolean = offset.until(offsetUntil).contains(index)

  def pathTo: List[SyntaxKind] = parent
    .value
    .foldMap { parent =>
      parent.pathTo ++
        parent.green.fold(_.kind.some, _ => None).toList
    }

  def findAt(
    index: Int
  ): Option[SyntaxNode] =
    if (includes(index))
      children.collectFirstSome(_.findAt(index)).orElse(this.some)
    else {
      None
    }

  /** Returns children of this node as a SyntaxNode representation. NOTE: direct children that are
    * tokens are not included: you'll only see children that are green nodes themselves.
    */
  def children: List[SyntaxNode] = {
    val rawChildren = green.fold(_.children, _ => Nil)
    val childOffsets = rawChildren.scanLeft[Int](offset)(_ + _.fold(_.width, _.width))

    rawChildren.zip(childOffsets).map {
      case (
            child,
            offset,
          ) =>
        SyntaxNode(offset, Eval.now(this.some), child)
    }
  }

  def print(
    showNodeTexts: Boolean = false
  ): String = {
    def go(
      depth: Int,
      self: SyntaxNode,
    ): String = {
      val content = self
        .green
        .fold(
          if (showNodeTexts)
            gn => ": \"" + sanitize(gn.allTokens.map(_.text).mkString) + "\""
          else
            _ => "",
          t => s" \"${t.text}\"",
        )
      "  " * depth +
        s"""${self.green.fold(_.kind, _.kind)}@${self.range._1}..${self.range._2}$content
           |""".stripMargin +
        self
          .children
          .map(go(depth + 1, _))
          .mkString
    }

    go(0, this)
  }

  private def sanitize(
    text: String
  ) = text.replace(" ", "·").replace("\n", "↵")

}

object SyntaxNode {

  def newRoot(
    green: GreenNode
  ): SyntaxNode = SyntaxNode(offset = 0, parent = Eval.now(None), green = green.asLeft)

}

sealed trait SyntaxKind extends Product with Serializable

object SyntaxKind {
  case object SourceFile extends SyntaxKind
  case object Decl extends SyntaxKind
  case object UseDecl extends SyntaxKind
  case object FQN extends SyntaxKind
  case object Namespace extends SyntaxKind
  case object Identifier extends SyntaxKind
  case object ArrayLiteral extends SyntaxKind
  case object ObjectLiteral extends SyntaxKind
  case object Expression extends SyntaxKind
  case object ERROR extends SyntaxKind
}

// trait AstNode[Self] { self: Product =>
//   def syntax: SyntaxNode

// def firstChildToken(
//   kind: TokenKind
// ): Option[Token] = syntax.children.collectFirst {
//   case SyntaxNode(_, _, Right(tok @ Token(`kind`, _))) => tok
// }

// def allChildNodes[N: AstNodeMirror]: List[N] = syntax.children.mapFilter(_.cast[N])

// def firstChildNode[N: AstNodeMirror]: Option[N] = syntax.children.collectFirstSome(_.cast[N])

// }

// trait AstNodeMirror[Self] {

//   def cast(
//     node: SyntaxNode
//   ): Option[Self]

// }

// object AstNodeMirror {

//   def instance[T](
//     matchingSyntaxKind: SyntaxKind
//   )(
//     make: SyntaxNode => T
//   ): AstNodeMirror[T] =
//     node =>
//       node.green.left.map(_.kind) match {
//         case Left(`matchingSyntaxKind`) => Some(make(node))
//         case _                          => None
//       }

// }

// concrete

// case class Identifier(
//   syntax: SyntaxNode
// ) extends AstNode[Identifier] {
//   def value: Option[Token] = firstChildToken(TokenKind.IDENT)
// }

// object Identifier {

//   implicit val node: AstNodeMirror[Identifier] =
//     AstNodeMirror.instance(SyntaxKind.Identifier)(apply)

// }

// case class Namespace(
//   syntax: SyntaxNode
// ) extends AstNode[Namespace] {
//   def parts: List[Identifier] = allChildNodes[Identifier]
// }

// object Namespace {

//   implicit val node: AstNodeMirror[Namespace] = AstNodeMirror.instance(SyntaxKind.Namespace)(apply)

// }

// case class FQN(
//   syntax: SyntaxNode
// ) extends AstNode[FQN] {
//   def namespace: Option[Namespace] = firstChildNode[Namespace]
//   def name: Option[Identifier] = firstChildNode[Identifier]
// }

// object FQN {

//   implicit val node: AstNodeMirror[FQN] = AstNodeMirror.instance(SyntaxKind.FQN)(apply)

// }

case class Tokens(
  private var all: List[Token],
  private var cursor: Int,
) {
  private var builder: GreenNode.GreenNodeBuilder = null

  def id: Int = cursor

  def eof: Boolean = cursor >= all.length
  def eofOrNewline: Boolean = cursor >= all.length || peek().kind == TokenKind.NEWLINE

  def peek(
  ): Token =
    try all(cursor)
    catch {
      case _: IndexOutOfBoundsException => sys.error("peeked into EOF!")
    }

  def bump(
  ): Token = {
    val result = peek()
    cursor += 1
    result
  }

  def setBuilder(
    builder: GreenNode.GreenNodeBuilder
  ) = this.builder = builder

  def eatErrorsUntilNewlineOr(
    tok: TokenKind
  ): Unit = eatErrorsUntilNewlineOr0(List(tok))

  def eatErrorsUntilNewlineOr0(
    toks: List[TokenKind]
  ): Unit =
    while (!eof && peek().kind != TokenKind.NEWLINE && !toks.contains(peek().kind)) {
      val next = bump()
      if (this.builder ne null)
        this.builder.addErrorToken(next)
      else
        sys.error("Fatal: no active builder while consuming errors")
    }

}

object Tokens {

  def apply(
    tokens: List[Token]
  ): Tokens = Tokens(tokens, 0)

}

sealed trait Error extends Product with Serializable

object Error {

  case class UnexpectedToken(
    token: Token
  ) extends Error

  case class MissingToken(
    kinds: NonEmptyList[TokenKind]
  ) extends Error

}

case class Parser(
  tokens: Tokens,
  var errors: List[Error],
) {

  def setBuilder(
    builder: GreenNode.GreenNodeBuilder
  ): Unit = tokens.setBuilder(builder)

  def addError(
    error: Error
  ): Unit = errors ::= error

}

object Parser {

  def init(
    tokens: List[Token]
  ): Parser = Parser(Tokens(tokens), errors = Nil)

  def fromString(
    s: String
  ): Parser = init(Scanner.scan(s))

}
