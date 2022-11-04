package playground.smithyql

// I _wish_ this was generated boilerplate.
trait ASTVisitor[F[_], T] {

  def apply(ast: AST[F]): T =
    ast match {
      case QueryOperationName(identifier, operationName) =>
        queryOperationName(identifier, operationName)
      case OperationName(text)                    => operationName(text)
      case UseClause(identifier)                  => useClause(identifier)
      case Prelude(useClause)                     => prelude(useClause)
      case Query(useClause, operationName, input) => query(useClause, operationName, input)
      case RunQuery(query)                        => runQuery(query)
      case SourceFile(prelude, statements)        => sourceFile(prelude, statements)
      case IntLiteral(value)                      => intLiteral(value)
      case Struct(fields)                         => struct(fields)
      case NullLiteral()                          => nullLiteral
      case Listed(values)                         => listed(values)
      case BooleanLiteral(value)                  => booleanLiteral(value)
      case StringLiteral(value)                   => stringLiteral(value)
    }

  def sourceFile(
    prelude: Prelude[F],
    statements: F[List[Statement[F]]],
  ): T

  def prelude(useClause: Option[F[UseClause[F]]]): T
  def runQuery(query: F[Query[F]]): T

  def useClause(identifier: F[QualifiedIdentifier]): T

  def query(
    @deprecated("use clauses are now located in the prelude section of the file", "")
    useClause: F[Option[UseClause[F]]],
    operationName: F[QueryOperationName[F]],
    input: F[Struct[F]],
  ): T

  def queryOperationName(
    identifier: Option[F[QualifiedIdentifier]],
    operationName: F[OperationName[F]],
  ): T

  def operationName(text: String): T

  def struct(fields: F[Struct.Fields[F]]): T
  def listed(values: F[List[F[InputNode[F]]]]): T

  def booleanLiteral(value: Boolean): T
  def stringLiteral(value: String): T
  def intLiteral(value: String): T
  def nullLiteral: T
}

object ASTVisitor {

  trait Default[F[_], T] extends ASTVisitor[F, T] {
    def default: T

    def sourceFile(prelude: Prelude[F], statements: F[List[Statement[F]]]): T = default
    def prelude(useClause: Option[F[UseClause[F]]]): T = default
    def runQuery(query: F[Query[F]]): T = default
    def useClause(identifier: F[QualifiedIdentifier]): T = default

    def query(
      useClause: F[Option[UseClause[F]]],
      operationName: F[QueryOperationName[F]],
      input: F[Struct[F]],
    ): T = default

    def queryOperationName(
      identifier: Option[F[QualifiedIdentifier]],
      operationName: F[OperationName[F]],
    ): T = default

    def operationName(text: String): T = default

    def struct(fields: F[Struct.Fields[F]]): T = default
    def listed(values: F[List[F[InputNode[F]]]]): T = default

    def booleanLiteral(value: Boolean): T = default
    def stringLiteral(value: String): T = default
    def intLiteral(value: String): T = default
    def nullLiteral: T = default

  }

}
