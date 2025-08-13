package playground.lsp

import cats.FlatMap
import cats.MonadThrow
import cats.data.Kleisli
import cats.effect.kernel.Async
import cats.kernel.Semigroup
import cats.parse.LocationMap
import cats.syntax.all.*
import cats.tagless.Derive
import cats.tagless.FunctorK
import cats.tagless.implicits.*
import cats.~>
import io.circe.Json
import playground.CompilationError
import playground.CompilationFailed
import playground.FileCompiler
import playground.FileRunner
import playground.OperationCompiler
import playground.PreludeCompiler
import playground.ServiceIndex
import playground.Uri
import playground.language
import playground.language.CodeLens
import playground.language.CodeLensProvider
import playground.language.CommandProvider
import playground.language.CommandResultReporter
import playground.language.CompletionItem
import playground.language.CompletionProvider
import playground.language.DefinitionProvider
import playground.language.DiagnosticProvider
import playground.language.DocumentSymbol
import playground.language.DocumentSymbolProvider
import playground.language.Feedback
import playground.language.FormattingProvider
import playground.language.TextDocumentProvider
import playground.language.TextEdit
import playground.lsp.buildinfo.BuildInfo
import playground.smithyql.Position
import playground.smithyql.SourceRange
import playground.types.*
import smithy4s.dynamic.DynamicSchemaIndex

trait LanguageServer[F[_]] {

  def initialize[A](workspaceFolders: List[Uri]): F[InitializeResult]

  def didChange(
    documentUri: Uri,
    newText: String,
  ): F[Unit]

  def didOpen(
    documentUri: Uri,
    text: String,
  ): F[Unit]

  def didSave(
    documentUri: Uri
  ): F[Unit]

  def didClose(
    documentUri: Uri
  ): F[Unit]

  def formatting(
    documentUri: Uri
  ): F[List[LSPTextEdit]]

  def completion(
    documentUri: Uri,
    position: LSPPosition,
  ): F[List[LSPCompletionItem]]

  def diagnostic(
    documentUri: Uri
  ): F[List[LSPDiagnostic]]

  def codeLens(
    documentUri: Uri
  ): F[List[LSPCodeLens]]

  def documentSymbol(
    documentUri: Uri
  ): F[List[LSPDocumentSymbol]]

  def didChangeWatchedFiles: F[Unit]

  def executeCommand(
    commandName: String,
    arguments: List[Json],
  ): F[Unit]

  def definition(
    documentUri: Uri,
    position: LSPPosition,
  ): F[List[LSPLocation]]

  // custom smithyql/runQuery LSP request
  def runFile(
    params: RunFileParams
  ): F[Unit]

}

object LanguageServer {

  def notAvailable[F[_]: MonadThrow]: LanguageServer[F] = defer(
    new Throwable("Server not available").raiseError[F, LanguageServer[F]]
  )

  def instance[
    F[_]: Async: TextDocumentManager: LanguageClient: ServerLoader: CommandResultReporter
  ](
    dsi: DynamicSchemaIndex,
    serviceIndex: ServiceIndex,
    runner: FileRunner.Resolver[F],
  ): LanguageServer[F] =
    new LanguageServer[F] {

      private val iorToF: IorThrow ~> F =
        new (IorThrow ~> F) {

          def apply[A](
            fa: IorThrow[A]
          ): F[A] = fa.toEither.liftTo[F]

        }

      val compiler: FileCompiler[IorThrow] = FileCompiler
        .instance(
          PreludeCompiler.instance[CompilationError.InIorNel](serviceIndex),
          OperationCompiler.fromSchemaIndex(dsi, serviceIndex),
        )
        .mapK(CompilationFailed.wrapK)

      val completionProvider: CompletionProvider = CompletionProvider.forSchemaIndex(
        dsi,
        serviceIndex,
      )
      val diagnosticProvider: DiagnosticProvider[F] = DiagnosticProvider.instance(compiler, runner)
      val lensProvider: CodeLensProvider[F] = CodeLensProvider.instance(compiler, runner)

      val commandProvider: CommandProvider[F] = CommandProvider
        .instance[F](compiler.mapK(iorToF), runner)

      private val getFormatterWidth: F[Int] = LanguageClient[F]
        .configuration(ConfigurationValue.maxWidth)

      val formattingProvider: Uri => F[List[language.TextEdit]] = FormattingProvider.provider(
        getFormatterWidth
      )

      val definitionProvider = DefinitionProvider.instance[F](serviceIndex)

      def initialize[A](workspaceFolders: List[Uri]): F[InitializeResult] = {
        def capabilities(compiler: ServerCapabilitiesCompiler): compiler.Result = {
          given Semigroup[compiler.Result] = compiler.semigroup
          compiler.textDocumentSync(TextDocumentSyncKind.Full) |+|
            compiler.documentFormattingProvider |+|
            compiler.completionProvider |+|
            compiler.diagnosticProvider |+|
            compiler.codeLensProvider |+|
            compiler.documentSymbolProvider |+|
            compiler.definitionProvider
        }

        val serverInfo = ServerInfo("Smithy Playground", BuildInfo.version)

        Feedback[F]
          .showInfoMessage(
            s"Hello from Smithy Playground v${BuildInfo.version}! Loading project..."
          ) *>
          ServerLoader[F]
            .prepare(workspaceFolders.some)
            .flatMap { prepped =>
              ServerLoader[F].perform(prepped.params).flatTap { stats =>
                Feedback[F]
                  .showInfoMessage(
                    s"Loaded Smithy Playground server with ${stats.render}"
                  )
              }
            }
            .onError { case e => LanguageClient[F].showErrorMessage("Failed to reload project") }
            .attempt
            .as(InitializeResult(capabilities, serverInfo))
      }

      def didChange(
        documentUri: Uri,
        newText: String,
      ): F[Unit] = TextDocumentManager[F].put(
        documentUri,
        newText,
      )

      def didOpen(
        documentUri: Uri,
        text: String,
      ): F[Unit] = TextDocumentManager[F].put(
        documentUri,
        text,
      )

      def didSave(
        documentUri: Uri
      ): F[Unit] = TextDocumentManager[F]
        .remove(documentUri)

      def didClose(
        documentUri: Uri
      ): F[Unit] = TextDocumentManager[F].remove(documentUri)

      def formatting(
        documentUri: Uri
      ): F[List[LSPTextEdit]] = TextDocumentProvider[F].get(documentUri).flatMap { doc =>
        val map = LocationMap(doc)
        formattingProvider(documentUri).map(_.map(LSPTextEdit(_, map)))
      }

      def completion(
        documentUri: Uri,
        position: LSPPosition,
      ): F[List[LSPCompletionItem]] = TextDocumentManager[F]
        .get(documentUri)
        .map { documentText =>
          val map = LocationMap(documentText)

          completionProvider
            .provide(
              documentText,
              position.unwrap(map),
            )
            .map(LSPCompletionItem(_, map))
        }

      def diagnostic(
        documentUri: Uri
      ): F[List[LSPDiagnostic]] = TextDocumentManager[F]
        .get(documentUri)
        .map { documentText =>
          val diags = diagnosticProvider.getDiagnostics(
            documentText
          )

          val map = LocationMap(documentText)

          diags
            .map(LSPDiagnostic(_, map))
        }

      def definition(documentUri: Uri, position: LSPPosition): F[List[LSPLocation]] =
        TextDocumentManager[F]
          .get(documentUri)
          .flatMap { documentText =>
            val map = LocationMap(documentText)

            definitionProvider
              .definition(documentUri, position.unwrap(map))
              .map {
                _.map { loc =>
                  LSPLocation(
                    document = loc.document,
                    range = LSPRange.from(loc.range),
                  )
                }
              }
          }

      def codeLens(
        documentUri: Uri
      ): F[List[LSPCodeLens]] = TextDocumentManager[F]
        .get(documentUri)
        .map { documentText =>
          val map = LocationMap(documentText)

          lensProvider
            .provide(
              documentUri = documentUri,
              documentText = documentText,
            )
            .map(LSPCodeLens(_, map))
        }

      def documentSymbol(
        documentUri: Uri
      ): F[List[LSPDocumentSymbol]] = TextDocumentManager[F]
        .get(documentUri)
        .map { text =>
          val map = LocationMap(text)

          DocumentSymbolProvider.make(text).map(LSPDocumentSymbol(_, map))
        }

      def didChangeWatchedFiles: F[Unit] = ServerLoader[F]
        .prepare(workspaceFolders = None)
        .flatMap {
          case prepared if !prepared.isChanged =>
            Feedback[F].showInfoMessage(
              "No change detected, not rebuilding server"
            )
          case prepared =>
            Feedback[F].showInfoMessage("Detected changes, will try to rebuild server...") *>
              ServerLoader[F]
                .perform(prepared.params)
                .onError { case e =>
                  LanguageClient[F].showErrorMessage(
                    "Couldn't reload server: " + e.getMessage
                  )
                }
                .flatMap { stats =>
                  LanguageClient[F].refreshDiagnostics *>
                    LanguageClient[F].refreshCodeLenses *>
                    Feedback[F].showInfoMessage(
                      s"Reloaded Smithy Playground server with ${stats.render}"
                    )
                }
        }
        .onError { case e =>
          LanguageClient[F].showErrorMessage(
            s"Couldn't rebuild server. Check your config file and the output panel.\nError: ${e.getMessage()}"
          )
        }

      def executeCommand(
        commandName: String,
        args: List[Json],
      ): F[Unit] = args
        .traverse(_.as[String].liftTo[F])
        .flatMap(commandProvider.runCommand(commandName, _))

      def runFile(
        params: RunFileParams
      ): F[Unit] = executeCommand(
        playground.language.Command.RUN_FILE,
        List(Json.fromString(params.uri.value)),
      )
    }

  implicit val functorK: FunctorK[LanguageServer] = Derive.functorK[LanguageServer]

  def defer[F[_]: FlatMap](
    fa: F[LanguageServer[F]]
  ): LanguageServer[F] = Derive
    .readerT[LanguageServer, F]
    .mapK(new (Kleisli[F, LanguageServer[F], *] ~> F) {

      def apply[A](
        k: Kleisli[F, LanguageServer[F], A]
      ): F[A] = fa.flatMap(k.run)

    })

}

enum TextDocumentSyncKind {
  case Full
}

// different encoding than usual, the main point is to force the caller to handle all methods
// (not sure how well that'll work with langoustine, but we'll see - I guess a lot of copying)
trait ServerCapabilitiesCompiler {
  type Result
  def semigroup: Semigroup[Result]

  def textDocumentSync(kind: TextDocumentSyncKind): Result
  def documentFormattingProvider: Result
  def completionProvider: Result
  def diagnosticProvider: Result
  def codeLensProvider: Result
  def documentSymbolProvider: Result
  def definitionProvider: Result
}

case class ServerInfo(name: String, version: String)

case class InitializeResult(
  serverCapabilities: (compiler: ServerCapabilitiesCompiler) => compiler.Result,
  serverInfo: ServerInfo,
)

case class LSPDiagnostic(diagnostic: CompilationError, map: LocationMap)
case class LSPCodeLens(lens: CodeLens, map: LocationMap)
case class LSPDocumentSymbol(sym: DocumentSymbol, map: LocationMap)
case class LSPCompletionItem(item: CompletionItem, map: LocationMap)
case class LSPTextEdit(textEdit: TextEdit, map: LocationMap)
case class LSPLocation(document: Uri, range: LSPRange)

case class LSPPosition(line: Int, character: Int) {

  def unwrap(map: LocationMap): Position = Position(map.toOffset(line, character).getOrElse(-1))

}

object LSPPosition {

  def from(position: Position, map: LocationMap): LSPPosition = {
    val caret = map.toCaretUnsafe(position.index)
    LSPPosition(caret.line, caret.col)
  }

}

case class LSPRange(from: LSPPosition, to: LSPPosition)

object LSPRange {

  def from(range: SourceRange, map: LocationMap): LSPRange = LSPRange(
    LSPPosition.from(range.start, map),
    LSPPosition.from(range.end, map),
  )

  def from(range: SourceRange.InFile): LSPRange = LSPRange(
    LSPPosition(range.start.lineOneIndexed, range.start.columnOneIndexed),
    LSPPosition(range.end.lineOneIndexed, range.end.columnOneIndexed),
  )

}
