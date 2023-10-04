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

  tokens.eatErrorsUntilNewlineOr(KW_USE, e => builder.addChild(GreenNode.error(e)))

  tokens.peek().kind match {
    case KW_USE =>
      // all good, continue
      builder.addChild(tokens.bump())
    case _ =>
      // USE was missing.
      state.addError(Error.MisingToken(TokenKind.KW_USE))
  }

  tokens.eatErrorsUntilNewlineOr(TokenKind.KW_SERVICE, e => builder.addChild(GreenNode.error(e)))

  tokens.peek().kind match {
    case KW_SERVICE =>
      // all good, continue
      builder.addChild(tokens.bump())
    case _ =>
      // SERVICE was missing.
      state.addError(Error.MisingToken(TokenKind.KW_SERVICE))
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
  tokens.peek().kind match {
    case TokenKind.KW_USE => builder.addChild(parseUseDecl(state))
    // case _                => builder.addChild(parseStatement(state))
  }
  builder.build()
}

def parseSourceFile(
  state: Parser
): GreenNode = {
  val builder = GreenNode.builder(SyntaxKind.SourceFile)
  while (!state.tokens.eof)
    builder.addChild(parseDecl(state))
  builder.build()
}

def parseIdent(
  state: Parser
): GreenNode = {
  val builder = GreenNode.builder(SyntaxKind.Identifier)
  val next = state.tokens.bump()
  next.kind match {
    case TokenKind.IDENT => builder.addChild(next)
    case _               => builder.addChild(GreenNode.error(next))
  }
  builder.build()
}

def parseNamespace(
  state: Parser
): GreenNode = {
  import state.tokens
  val builder = GreenNode.builder(SyntaxKind.Namespace)

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
          List(TokenKind.DOT, TokenKind.HASH),
          e => builder.addChild(GreenNode.error(e)),
        )

        if (!tokens.eof) {
          if (tokens.peek().kind == TokenKind.HASH) {
            done = true
          } else if (tokens.peek().kind == TokenKind.DOT) {
            builder.addChild(tokens.bump())
          } else {
            // no dot, report an error but continue (maybe there's a hash ahead, for the next iteration)
            state.addError(Error.MisingToken(TokenKind.DOT))
          }
        }

      } else {
        // we don't have an ident, so report an error
        state.addError(Error.MisingToken(TokenKind.IDENT))
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

  builder.addChild(parseNamespace(state))

  tokens.eatErrorsUntilNewlineOr(TokenKind.HASH, e => builder.addChild(GreenNode.error(e)))

  if (tokens.peek().kind == TokenKind.HASH) {
    builder.addChild(tokens.bump())
  } else {
    state.addError(Error.MisingToken(TokenKind.HASH))
  }

  // the rest of the line should be an ident
  builder.addChild(parseIdent(state))

  builder.build()
}

// val p = Parser.fromString("use service foo.bar#baz")
val p = Parser.fromString("foo.bar#baz")
SyntaxNode.newRoot(parseFQN(p)).print

p.tokens.eof
p.errors
