import cats.data.NonEmptyList
import cats.implicits._
import playground.smithyql.parser.v2._
import playground.smithyql.parser.v2.scanner.TokenKind.KW_SERVICE
import playground.smithyql.parser.v2.scanner.TokenKind.KW_USE
import playground.smithyql.parser.v2.scanner._
// sourceFile = decl*
// decl = useDecl | statement
// useDecl = "use" "service" fqn
// statement = query
// query = queryOperationName struct
// queryOperationName = (qualifiedIdent ".")? operationName
// operationName = ident
// qualifiedIdent = fqn
// fqn = namespace # ident
// namespace = ident ("." ident)*
// struct = "{" fields? "}"
// fields = field ("," field)* ("," | )
// field = ident ":" node
// node = struct | list | literal
// list = "[" nodes? "]"
// nodes = node ("," node)* ("," | )

def parseUseDecl(
  state: Parser
): GreenNode = {
  import state.tokens

  val builder = GreenNode.builder(SyntaxKind.UseDecl)
  state.setBuilder(builder)

  tokens.eatErrorsUntilNewlineOr(KW_USE)

  if (!tokens.eof)
    tokens.peek().kind match {
      case KW_USE =>
        // all good, continue
        builder.addChild(tokens.bump())
      case _ =>
        // USE was missing.
        state.addError(Error.MissingToken(TokenKind.KW_USE.pure[NonEmptyList]))
    }

  tokens.eatErrorsUntilNewlineOr(TokenKind.KW_SERVICE)

  if (!tokens.eof)
    tokens.peek().kind match {
      case KW_SERVICE =>
        // all good, continue
        builder.addChild(tokens.bump())
      case _ =>
        // SERVICE was missing.
        state.addError(Error.MissingToken(TokenKind.KW_SERVICE.pure[NonEmptyList]))
    }

  // we've gone past the need for keywords, time to eat a FQN
  builder.addChild(parseFQN(state))

  builder.build()
}

def parseDecl(
  state: Parser
): GreenNode = {
  import state.tokens
  val builder = GreenNode.builder(SyntaxKind.Decl)
  state.setBuilder(builder)
  tokens.peek().kind match {
    case TokenKind.KW_USE
        // unhappy path, but still needs to be taken
        | TokenKind.KW_SERVICE =>
      builder.addChild(parseUseDecl(state))
    case _ => tokens.eatErrorsUntilNewlineOr0(Nil)
    // case _                => builder.addChild(parseStatement(state))
  }
  builder.build()
}

def parseSourceFile(
  state: Parser
): GreenNode = {
  val builder = GreenNode.builder(SyntaxKind.SourceFile)
  state.setBuilder(builder)
  while (!state.tokens.eof)
    builder.addChild(parseDecl(state))
  builder.build()
}

def parseIdent(
  state: Parser
): GreenNode = {
  val builder = GreenNode.builder(SyntaxKind.Identifier)
  state.setBuilder(builder)
  state.tokens.eatErrorsUntilNewlineOr(TokenKind.IDENT)

  if (!state.tokens.eof) {
    val next = state.tokens.bump()
    next.kind match {
      case TokenKind.IDENT => builder.addChild(next)
      case _               => builder.addChild(GreenNode.error(next))
    }
  }

  builder.build()
}

def parseNamespace(
  state: Parser
): GreenNode = {
  import state.tokens
  val builder = GreenNode.builder(SyntaxKind.Namespace)
  state.setBuilder(builder)

  var done = false

  while (!tokens.eofOrNewline && !done)
    checkedLoop(tokens.id) {
      tokens.peek().kind match {
        case TokenKind.HASH =>
          // end of namespace, move on.
          // Could be that the namespace is empty, technically an error, should we report?
          done = true

        case TokenKind.IDENT => // will be captured in the next match
        case _               =>
          // this is an error, unless it's whitespace.
          builder.addChild(GreenNode.error(tokens.bump()))
      }

      if (!done) {
        // we have an ident, so parse it
        builder.addChild(parseIdent(state))
        // look for a dot... or hash
        tokens.eatErrorsUntilNewlineOr0(
          List(TokenKind.DOT, TokenKind.HASH)
        )

        if (!tokens.eof) {
          if (tokens.peek().kind == TokenKind.HASH) {
            done = true
          } else if (tokens.peek().kind == TokenKind.DOT) {
            builder.addChild(tokens.bump())
          } else {
            // no dot, report an error but continue (maybe there's a hash ahead, for the next iteration)
            state.addError(Error.MissingToken(TokenKind.DOT.pure[NonEmptyList]))
          }
        }

      } else {
        // we don't have an ident, so report an error
        state.addError(Error.MissingToken(TokenKind.IDENT.pure[NonEmptyList]))
      }
    }

  builder.build()
}

def checkedLoop[A](
  check: => A
)(
  loop: => Unit
): Unit = {
  val start = check
  loop
  val end = check
  if (start == end)
    sys.error("loop did not advance!")
}

def parseFQN(
  state: Parser
): GreenNode = {
  import state.tokens
  val builder = GreenNode.builder(SyntaxKind.FQN)
  state.setBuilder(builder)

  builder.addChild(parseNamespace(state))

  tokens.eatErrorsUntilNewlineOr(TokenKind.HASH)

  if (!tokens.eof && tokens.peek().kind == TokenKind.HASH) {
    builder.addChild(tokens.bump())
  } else {
    state.addError(Error.MissingToken(TokenKind.HASH.pure[NonEmptyList]))
  }

  // the rest of the line should be an ident
  builder.addChild(parseIdent(state))

  builder.build()
}

def test(
  s: String,
  f: Parser => GreenNode,
) = {
  val sn = SyntaxNode.newRoot(f(Parser.fromString(s)))

  // println(sn.print)
  val isTrippable = sn.green.fold(gn => gn.allTokens, List(_)).map(_.text).mkString == s
  def errors(
    gn: Either[GreenNode, Token]
  ): List[String] = gn.fold(
    g =>
      (if (g.kind == SyntaxKind.ERROR)
         Some(g.allTokens.map(_.kind.toString()).mkString(","))
       else
         None).toList ++ g.children.flatMap(errors),
    tok =>
      if (tok.kind == TokenKind.Error)
        List(tok.kind.toString())
      else
        Nil,
  )

  require(isTrippable, s"input not trippable! $s")
  val errs = errors(sn.green)
  if (errs.size > 0)
    s"ERRORS: ${errs}"
  else
    ()
}

test("foo.barbaz", parseFQN)
test("foo.bar#baz", parseFQN)
test("a", parseIdent)
test("42", parseIdent)
test("a.42", parseNamespace)
test("a.42#b", parseNamespace)
test("use service a.42#baz", parseSourceFile)
test("use a.42#baz", parseSourceFile)
test("a.42#baz", parseSourceFile)

// val p = Parser.fromString("use service foo.bar#baz")
// val p = Parser.fromString("foo.42#baz")
// val rt = SyntaxNode.newRoot(parseFQN(p))

// rt.print

// p.tokens.eof
// p.errors

sealed trait SyntaxPart[A] {
  type Tpe = A

  def ? : SyntaxPart[Option[A]] = OptionalPart(this)

  def map[B](
    f: A => B
  ): SyntaxPart[B] = MapPart(this, f)

  def ofLeft[B]: SyntaxPart[Either[A, B]] = map(Left(_))
  def ofRight[B]: SyntaxPart[Either[B, A]] = map(Right(_))

  def print: String = SyntaxPart.printRec(0, this, Set.empty)

}

object SyntaxPart {

  private def printRec[A](
    depth: Int,
    self: SyntaxPart[A],
    seenDefers: Set[SyntaxPart[_]],
  ): String = {
    val prefix = "  " * depth

    self match {
      case DeferPart(_) => sys.error("printing defers is currently broken and illegal")
      // if (seenDefers.contains(self))
      //   prefix + s"Defer(rec)\n"
      // else
      //   prefix + s"Defer:\n" + printRec(depth + 1, part(), seenDefers + self)
      case OptionalPart(part) => prefix + s"Optional:\n" + printRec(depth + 1, part, seenDefers)
      case LoopPart(part)     => prefix + s"Loop:\n" + printRec(depth + 1, part, seenDefers)
      case MapPart(part, _)   => printRec(depth, part, seenDefers)
      case SyntaxRule(kind, part) =>
        prefix + s"Rule($kind):\n" + printRec(depth + 1, part, seenDefers)
      case TokenPart(token) => prefix + s"Token($token)\n"
      case GroupPart(parts) => parts.map(printRec(depth, _, seenDefers)).mkString_("")
      case OneOfPart(parts) =>
        prefix + s"OneOf:\n" + parts.map(printRec(depth + 1, _, seenDefers)).mkString_("")
    }
  }

}

case class DeferPart[A](
  part: (
  ) => SyntaxPart[A]
) extends SyntaxPart[A]

case class OptionalPart[A](
  part: SyntaxPart[A]
) extends SyntaxPart[Option[A]]

case class MapPart[A, B](
  part: SyntaxPart[A],
  f: A => B,
) extends SyntaxPart[B]

case class TokenPart(
  token: NonEmptyList[TokenKind]
) extends SyntaxPart[List[Token]]

case class LoopPart[A](
  part: SyntaxPart[A]
) extends SyntaxPart[List[A]]

case class GroupPart[A](
  parts: NonEmptyList[SyntaxPart[A]]
) extends SyntaxPart[NonEmptyList[A]]

case class OneOfPart(
  parts: NonEmptyList[SyntaxPart[GreenNode]]
) extends SyntaxPart[GreenNode]

case class SyntaxRule(
  kind: SyntaxKind,
  part: SyntaxPart[List[Either[GreenNode, Token]]],
) extends SyntaxPart[GreenNode]

def interpret: SyntaxPart[GreenNode] => List[Token] => GreenNode = {

  case class State(
    tokens: List[Token],
    errors: List[Error],
  )
  type S[A] = cats.data.State[State, A]
  object S {
    // get errors and clear them in state
    val capture: S[List[Error]] =
      cats.data.State.inspect((_: State).errors) <*
        setErrors(Nil)

    val get: S[List[Token]] = cats.data.State.inspect(_.tokens)
    private val getAll: S[State] = cats.data.State.get
    private def setAll(
      s: State
    ): S[Unit] = cats.data.State.set(s)
    def error(
      t: Error
    ): S[Unit] = S.getAll.map(_.errors).map(_.appended(t)).flatMap(setErrors)
    private def setErrors(
      errors: List[Error]
    ): S[Unit] = cats.data.State.modify(s => s.copy(errors = errors))
    def set(
      tokens: List[Token]
    ): S[Unit] = cats.data.State.modify(s => s.copy(tokens = tokens))
    private val consumeTrivia: S[List[Token]] = cats.data.State { state =>
      val (triviaTokens, rest) = state.tokens.span { tok =>
        Set(TokenKind.NEWLINE, TokenKind.SPACE, TokenKind.COMMENT).contains(tok.kind)
      }

      (state.copy(tokens = rest), triviaTokens)
    }
    private def dryRun[A](
      sa: S[A]
    ): S[A] = cats.data.State { s =>
      val result = sa.runA(s).value

      (s, result)
    }

    val consumeOne: S[
      (
        Token,
        List[Token],
      )
    ] =
      (
        consumeTrivia,
        get.flatMap {
          case Nil          => Token(TokenKind.EOF, "").pure[S]
          case head :: tail => set(tail).as(head)
        },
      ).tupled.map(_.swap)

    val peekOne = dryRun(consumeOne)

    def modify(
      f: List[Token] => List[Token]
    ): S[Unit] = get.map(f).flatMap(set)
    def apply[A](
      f: List[Token] => (
        List[Token],
        A,
      )
    ): S[A] =
      (get.map(f), getAll).mapN { case ((tokens, a), s) =>
        setAll(s.copy(tokens = tokens)).as(a)
      }.flatten

    val unit: S[Unit] = cats.data.State.pure(())
  }

  def nextToken[A](
    part: SyntaxPart[A]
  ): NonEmptyList[TokenKind] =
    part match {
      case DeferPart(part) =>
        // this is only possible because L-recursion is illegal...
        // in theory. Nobody's checking.
        nextToken(part())
      case OptionalPart(part)  => nextToken(part)
      case GroupPart(parts)    => nextToken(parts.head)
      case MapPart(part, _)    => nextToken(part)
      case SyntaxRule(_, part) => nextToken(part)
      case LoopPart(part)      => nextToken(part)

      case TokenPart(token) => token
      case OneOfPart(parts) => parts.flatMap(nextToken)
    }

  def loop[A](
    current: SyntaxPart[A]
  ): S[A] = {
    def debug(
      s: => String
    ): S[Unit] = S { tokens =>
      println(s"""DEBUG: $s
                 |TOKENS=${tokens.map(_.kind).mkString(", ")}
                 |""".stripMargin)
      (tokens, ())
    }

    current match {
      case OptionalPart(part) =>
        S.consumeOne.flatMap {
          case (tok, _) if tok.isEof => S.unit.as(None)
          case _                     => loop(part).map(_.some)
        }

      case DeferPart(part)  => loop(part())
      case MapPart(self, f) => loop(self).map(f)
      case OneOfPart(parts) =>
        /** ok, so the general idea is:
          *
          * in this design, the first token is enough to determine which branch to take. We will
          * assume that all parts begin (unwrapped) with a token. We'll take that token for each
          * branch, skip trivia and see which branch matches such a token. Then we rewind and run
          * the branch normally through the loop.
          */
        parts
          .findM[S] { part =>
            val possibleTokens = nextToken(part)
            S.peekOne.map { case (tok, _) => possibleTokens.contains_(tok.kind) }
          }
          .flatMap {
            // found a part, go with it!
            case Some(part) => loop(part)
            case None =>
              val allPossibleParts = parts.flatMap(nextToken).distinct

              GreenNode
                .builder(SyntaxKind.ERROR)
                .addError(Error.MissingToken(allPossibleParts))
                .build()
                .pure[S]
          }
      case GroupPart(parts) =>
        /** Token parts are sync points. Before we try to parse the first part (and each subsequent
          * part actually), we need to determine if that part is immediately followed by a sync
          * point. If so, we have to find the sync point in the list of tokens and perform a split.
          * The initial part will then be used to parse this part. Afterwards, any leftover tokens
          * will be restored to the token buffer, and parsing will continue with the next part.
          *
          * If we're at the last part, we don't need to do this.
          */
        parts.zipWithIndex.traverse { case (part, i) =>
          val expectedTokensOpt = parts.toList.lift(i + 1).map(nextToken(_))

          expectedTokensOpt match {
            case None =>
              // no parts afterwards - we consume normally
              loop(part)
            case Some(possibleTokens) =>
              debug(s"looking for one of ${possibleTokens.mkString_(", ")}") *>
                S.get.flatMap { tokens =>
                  tokens.indexWhere(tok => possibleTokens.contains_(tok.kind)) match {
                    case -1 =>
                      debug("next part token not found, proceeding normally") *>
                        loop(part)
                    case idx =>
                      debug(s"found at $idx, scoping...") *> {
                        val (before, after) = tokens.splitAt(idx)
                        S.set(before) *>
                          loop(part) <*
                          S.modify(_ ++ after)
                      }
                  }
                }
          }
        }

      case SyntaxRule(kind, part) =>
        debug(s"starting rule $kind") *>
          (loop(part), S.capture).mapN {
            (
              children,
              childErrors,
            ) =>
              println("STOLEN: " + childErrors)
              val builder = GreenNode.builder(kind)
              builder.addAll(children)
              childErrors.foreach(builder.addError(_))
              builder.build()
          }

      case lp: LoopPart[a] =>
        debug(s"starting loop of ${lp.part}") *>
          S.peekOne.flatMap {
            case (tok, _) if tok.isEof => S.unit.as(Nil)
            case _                     => (loop(lp.part), loop(current)).mapN(_ :: _)
          }

      case TokenPart(expectedTokens) =>
        debug(s"starting token $expectedTokens") *>
          S.consumeOne.flatMap {
            case (tok, trivia) if tok.isEof => S.unit.as(trivia)
            case (tok, trivia) =>
              if (expectedTokens.contains_(tok.kind))
                trivia.appended(tok).pure[S]
              else
                S.error(Error.MissingToken(expectedTokens)).as(trivia.appended(tok))
          }

    }
  }
  part =>
    tokens =>
      loop(part)
        .run(State(tokens = tokens, errors = Nil))
        .map { case (state, result) =>
          println(s"RESULT(raw): $result")
          println(s"LEFTOVER TOKENS: ${state.tokens}")
          require(state.errors.isEmpty, s"found uncaptured errors! ${state.errors.mkString(", ")}")
          result.toBuilder.addAll(state.tokens.map(GreenNode.error(_).asLeft)).build()
        }
        .value
}

def loop[A](
  part1: SyntaxPart[A],
  rest: SyntaxPart[A]*
): SyntaxPart[List[A]] = LoopPart(group(part1, rest: _*))
  .map(_.flatMap(_.toList))

def deferred[A](
  part: => SyntaxPart[A]
): SyntaxPart[A] = DeferPart(
  (
  ) => part
)

def syntax(
  kind: SyntaxKind
)(
  part1: SyntaxPart[List[Either[GreenNode, Token]]],
  rest: SyntaxPart[List[Either[GreenNode, Token]]]*
): SyntaxPart[GreenNode] = SyntaxRule(
  kind,
  group(part1, rest: _*).map(_.toList.flatten),
)

def oneOf(
  part1: SyntaxPart[GreenNode],
  rest: SyntaxPart[GreenNode]*
): SyntaxPart[GreenNode] = OneOfPart(NonEmptyList(part1, rest.toList))

def tok(
  first: TokenKind,
  more: TokenKind*
): SyntaxPart[List[Either[GreenNode, Token]]] = TokenPart(NonEmptyList(first, more.toList))
  .map(_.toList.map(_.asRight))

def green(
  green: SyntaxPart[GreenNode]
): SyntaxPart[List[Either[GreenNode, Token]]] = green.map(v => List(v.asLeft))

def group[A](
  part1: SyntaxPart[A],
  rest: SyntaxPart[A]*
): SyntaxPart[NonEmptyList[A]] = GroupPart(NonEmptyList(part1, rest.toList))

val parseIdent2 = syntax(SyntaxKind.Identifier)(tok(TokenKind.IDENT))

val parseNamespace2 =
  syntax(SyntaxKind.Namespace)(
    green(parseIdent2),
    loop(
      tok(TokenKind.DOT),
      green(parseIdent2),
    ).map(_.flatten),
  )

val parseFQN2 =
  syntax(SyntaxKind.FQN)(
    green(parseNamespace2),
    tok(TokenKind.HASH),
    green(parseIdent2),
  )

val parseUseDecl2 =
  syntax(SyntaxKind.UseDecl)(
    tok(TokenKind.KW_USE),
    tok(TokenKind.KW_SERVICE),
    green(parseFQN2),
  )

val parseStringLiteral =
  syntax(SyntaxKind.StringLiteral)(
    tok(TokenKind.LIT_STRING)
  )

val parseNumericLiteral =
  syntax(SyntaxKind.NumericLiteral)(
    tok(TokenKind.LIT_NUMBER)
  )

val parseBooleanLiteral =
  syntax(SyntaxKind.BooleanLiteral)(
    tok(TokenKind.KW_BOOLEAN)
  )

val parseNullLiteral =
  syntax(SyntaxKind.NullLiteral)(
    tok(TokenKind.KW_NULL)
  )

val parseExpr: SyntaxPart[GreenNode] =
  syntax(SyntaxKind.Expression)(
    green(
      oneOf(
        deferred(parseArray),
        deferred(parseObject),
        parseStringLiteral,
        parseNumericLiteral,
        parseBooleanLiteral,
        parseNullLiteral,
      )
    )
  )

// comma-separated entries of "rule", with optional trailing comma
def commaSeparatedWithTrailing[A](
  rule: SyntaxPart[GreenNode]
): SyntaxPart[List[Either[GreenNode, Token]]] = group(
  green(rule),
  group(
    tok(TokenKind.COMMA),
    deferred(commaSeparatedWithTrailing(rule)),
  ).?.map(_.foldMap(_.toList.flatten)),
).?.map(_.foldMap(_.toList.flatten))

// listed: '[' (node (',' node)* (',')?)? ']';
val parseArray =
  syntax(SyntaxKind.ArrayLiteral)(
    tok(TokenKind.LB),
    commaSeparatedWithTrailing(parseExpr),
    tok(TokenKind.RB),
  )

val parseField =
  syntax(SyntaxKind.ObjectElem)(
    green(parseIdent2),
    tok(TokenKind.COLON, TokenKind.EQ),
    green(parseExpr),
  )

// struct: '{' (field (',' field)* (',')?)? '}';
val parseObject =
  syntax(SyntaxKind.ObjectLiteral)(
    tok(TokenKind.LBR),
    commaSeparatedWithTrailing(parseField),
    tok(TokenKind.RBR),
  )

interpret(parseField)(Scanner.scan("hello: 40")).syntax.print()
test2("hello: 40", interpret(parseField))
interpret(parseBooleanLiteral)(Scanner.scan("true")).syntax.print()
test2("true", interpret(parseBooleanLiteral))

interpret(parseObject)(Scanner.scan("{hello: 40}")).syntax.print()
test2("{hello: 40}", interpret(parseObject))

def test2(
  s: String,
  f: List[Token] => GreenNode,
) = {
  val sn = SyntaxNode.newRoot(f(Scanner.scan(s)))

  val rendered = sn.green.fold(gn => gn.allTokens, List(_)).map(_.text).mkString
  val isTrippable = rendered == s
  def errors(
    gn: Either[GreenNode, Token]
  ): List[String] = gn.fold(
    g =>
      (if (g.kind === SyntaxKind.ERROR)
         Some(g.allTokens.map(_.kind.toString()).mkString(","))
       else
         None).toList ++ g.children.flatMap(errors),
    tok =>
      if (tok.kind === TokenKind.Error)
        List(tok.kind.toString())
      else
        Nil,
  )

  require(isTrippable, s"input not trippable! $s != $rendered")
  val errs = errors(sn.green)
  if (errs.size > 0)
    s"ERRORS: ${errs}"
  else
    ()
}

test2("foo.bar.baz#baz", interpret(parseFQN2))
test2("foo.bar.bazb#a.z", interpret(parseFQN2))

test2("#", interpret(parseIdent2))
test2("###", interpret(parseIdent2))
test2("hello world\nlmao // elo elo 320\nfoo", interpret(parseIdent2))
test2("hello world\nlmao // elo elo 320\nfoo", interpret(parseNamespace2))
test2("hello world\nlmao // elo elo 320\nfoo", interpret(parseFQN2))

SyntaxNode.newRoot(interpret(parseFQN2)(Scanner.scan("hello world"))).print(true)
SyntaxNode
  .newRoot(interpret(parseFQN2)(Scanner.scan("hello world")))
  .findAt(10)
  .get
  .pathTo
// .pathTo

test2("use foo.bar#baz", interpret(parseUseDecl2))
test2("use service foo.bar#baz", interpret(parseUseDecl2))
test2("service foo.bar#baz", interpret(parseUseDecl2))
test2("use //aaservice foo.bar#baz", interpret(parseUseDecl2))

SyntaxNode
  .newRoot(interpret(parseUseDecl2)(Scanner.scan("use //service\nservice foo.bar#baz")))
  .findAt(22)
  .get
  .pathTo

SyntaxNode
  .newRoot(interpret(parseFQN2)(Scanner.scan("foo.[].dupa#baz")))
  .print(showNodeTexts = true)

// parseExpr.print

test2("{}", interpret(parseExpr))

interpret(
  syntax(SyntaxKind.ObjectLiteral)(
    loop(
      tok(TokenKind.IDENT),
      tok(TokenKind.COMMA).?.map(_.orEmpty),
    ).map(_.toList.flatten)
  )
).apply(Scanner.scan("aha,baha,caha,")).syntax.print()

interpret(
  parseObject
).apply(Scanner.scan("{aha: 42,baha: 43,caha: 45}")).syntax.print()

test2("{ foo : 42 }", interpret(parseExpr))
test2("{ foo : 42, bar : \"boo\", }", interpret(parseExpr))

// SyntaxNode
//   .newRoot(interpret(parseExpr)(Scanner.scan("{ foo : 42, bar : \"boo\", }")))
//   .print(true)

// test2("[]", interpret(parseExpr))

// SyntaxNode.newRoot(interpret(parseExpr)(Scanner.scan("{}"))).print(true)
// SyntaxNode.newRoot(interpret(parseExpr)(Scanner.scan("[]"))).print(true)

// test2("-", interpret(parseExpr))
// SyntaxNode.newRoot(interpret(parseExpr)(Scanner.scan("-"))).print(true)
