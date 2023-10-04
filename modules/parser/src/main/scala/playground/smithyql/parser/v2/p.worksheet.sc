import cats.implicits._
import playground.smithyql.parser.v2._
import playground.smithyql.parser.v2.scanner._

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

  while (!tokens.eof && !done)
    tokens.peek().kind match {
      case TokenKind.IDENT =>
        // todo: after an ident, expect dot or hash (some sort of state machine / another method in the recursive descent?)
        // if it's an ident, report an error but don't wrap in ERROR
        // otherwise, wrap in ERROR
        builder.addChild(parseIdent(state)): Unit

      case TokenKind.DOT =>
        // swallow token
        builder.addChild(tokens.bump()): Unit

      case TokenKind.HASH => done = true // end of namespace, move on

      case _ =>
        // skip extra/invalid tokens. we will report these in the future
        builder.addChild(GreenNode.error(tokens.bump()))
        tokens.bump(): Unit
    }

  builder.build()
}

def parseFQN(
  state: Parser
): GreenNode = {
  import state.tokens
  val builder = GreenNode.builder(SyntaxKind.FQN)
  builder.addChild(parseNamespace(state))
  if (tokens.peek().kind == TokenKind.HASH) {
    builder.addChild(tokens.bump())
  }
  builder.addChild(parseIdent(state))
  builder.build()
}

SyntaxNode
  .newRoot(parseIdent(Parser.init(TokenKind.IDENT("hello") :: Nil)))
  .cast[Identifier]
  .get
  .value

parseIdent(Parser.init(TokenKind.IDENT("hello") :: TokenKind.IDENT("world") :: Nil))

parseNamespace(Parser.init(Nil))
parseNamespace(Parser.init(TokenKind.IDENT("hello") :: Nil))

SyntaxNode
  .newRoot(parseNamespace(Parser.init(Scanner.scan("com.kubukoz.world"))))
  .cast[Namespace]
  .get
  .parts
  .map(_.value)

val fqn = SyntaxNode.newRoot(parseFQN(Parser.fromString("com.kubukoz#foo"))).cast[FQN]
fqn.get.namespace.get.parts.map(_.value.get)
fqn.get.name.get.value.get

//todo: this should have all tokens, even extraneous ones. Should render to the string above.
parseFQN(Parser.fromString("co111m.kub1ukoz#shrek_blob---,_,r")).allTokens.foldMap(_.text)

parseFQN(Parser.fromString("co111m.kub1ukoz#shrek_blob---,_,r"))
parseFQN(Parser.fromString("co111m.kub1ukoz#shrek_blob---,_,r")).print

val text = "com.kubukoz#helloworld"
pprint.pprintln(Scanner.scan(text))
pprint.pprintln(parseFQN(Parser.fromString(text)))
pprint.pprintln(SyntaxNode.newRoot(parseFQN(Parser.fromString(text))))
// pprint.pprintln(SyntaxNode.newRoot(parseFQN(Parser.fromString(text))).children)
println(SyntaxNode.newRoot(parseFQN(Parser.fromString(text))).print)
println(
  SyntaxNode
    .newRoot(parseFQN(Parser.fromString(text)))
    .findAt("com.kubukoz#h".length)
    .get
    .pathTo
)
