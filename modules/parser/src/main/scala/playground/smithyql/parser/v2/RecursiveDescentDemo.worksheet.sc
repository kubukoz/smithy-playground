import scala.reflect.ClassTag
/* vocab:

DOT: '.';
USE: 'use';
SERVICE: 'service';
HASH: '#';
ID: [a-zA-Z][a-zA-Z_0-9]*;

 */

/* grammar:

namespace: (ID ('.' ID)*);
qualified_identifier: namespace '#' ID;
use_clause: 'use' 'service' qualified_identifier;

source_file: use_clause* EOF;

 */

sealed trait Token extends Product with Serializable

abstract class SimpleToken(
  name: String
) extends Token
  with PartialFunction[Token, Unit] {

  override def toString(
  ): String = name

  override def isDefinedAt(
    x: Token
  ): Boolean = x == this

  override def apply(
    v1: Token
  ): Unit = ()

}

case object Dot extends SimpleToken("Dot")
case object Use extends SimpleToken("Use")
case object Service extends SimpleToken("Service")
case object Hash extends SimpleToken("Hash")

case class Id(
  value: String
) extends Token

def tokenize(
  input: String
): List[Token] = {
  // split by whitespace or punctuation, make sure punctuation is its own token
  val tokens = input.split("\\s+|(?=[#\\.])|(?<=[#\\.])(?!\\s)").toList
  tokens.flatMap {
    case "use"     => List(Use)
    case "service" => List(Service)
    case "#"       => List(Hash)
    case "."       => List(Dot)
    case id        => List(Id(id))
  }
}

var tokens = List.empty[Token]

def previewToken(
) = tokens.headOption

def expectTyped[T <: Token: ClassTag](
): T = expect(TypedMatcher[T](scala.reflect.classTag[T]))

case class TypedMatcher[T](
  ct: ClassTag[T]
) extends PartialFunction[Token, T] {

  override def apply(
    v1: Token
  ): T = ct.unapply(v1).get

  override def isDefinedAt(
    x: Token
  ): Boolean = ct.runtimeClass.isInstance(x)

}

def expect[T](
  t: PartialFunction[Token, T]
): T =
  previewToken().flatMap { tok =>
    nextToken(): Unit
    t.lift(tok)
  } match {
    case Some(p) => p
    case p       => throw new Exception(s"expected $t, got $p")
  }

def nextToken(
) = {
  val t = tokens.head
  tokens = tokens.tail
  t
}

case class QualifiedIdentifier(
  namespace: List[Id],
  service: Id,
)

case class UseClause(
  ident: QualifiedIdentifier
)

case class SourceFile(
  clauses: List[UseClause]
)

def qualifiedIdentifier: QualifiedIdentifier = {
  val initId = expectTyped[Id]()
  var namespace = List(initId)

  while (previewToken().contains(Dot)) {
    expect(Dot): Unit
    namespace = namespace :+ expectTyped[Id]()
  }

  expect(Hash): Unit

  val service = expectTyped[Id]()

  QualifiedIdentifier(namespace, service)
}

def useClause: UseClause = {
  expect(Use): Unit
  expect(Service): Unit
  UseClause(qualifiedIdentifier)
}

def sourceFile: SourceFile = {
  var clauses = List.empty[UseClause]

  while (previewToken().contains(Use))
    clauses = clauses :+ useClause

  SourceFile(clauses)
}

val example =
  """use service foo.bar#Baz
    |use service foo.bar#Quux""".stripMargin

tokens = tokenize(example)
tokens
sourceFile
