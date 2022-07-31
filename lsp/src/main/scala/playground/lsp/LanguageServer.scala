package playground.lsp

import cats.Applicative
import cats.FlatMap
import cats.MonadThrow
import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.effect.std.Supervisor
import cats.implicits._
import cats.tagless.Derive
import cats.tagless.SuspendK
import cats.tagless.Suspended
import cats.~>
import com.google.gson.JsonElement
import org.eclipse.lsp4j.ServerCapabilities
import org.eclipse.lsp4j.TextDocumentSyncKind
import org.eclipse.lsp4j._
import playground.CodeLensProvider
import playground.CommandProvider
import playground.CompletionProvider
import playground.DiagnosticProvider
import playground.Runner
import playground.TextDocumentManager
import playground.TextDocumentProvider
import playground.smithyql.Formatter
import playground.smithyql.SmithyQLParser
import playground.types._
import smithy4s.dynamic.DynamicSchemaIndex

import scala.jdk.CollectionConverters._
import scala.util.chaining._

trait LanguageServer[F[_]] {
  def initialize(params: InitializeParams): F[InitializeResult]
  def initialized(params: InitializedParams): F[Unit]
  def didChange(params: DidChangeTextDocumentParams): F[Unit]
  def didOpen(params: DidOpenTextDocumentParams): F[Unit]
  def didSave(params: DidSaveTextDocumentParams): F[Unit]
  def didClose(params: DidCloseTextDocumentParams): F[Unit]
  def formatting(params: DocumentFormattingParams): F[List[TextEdit]]
  def completion(position: CompletionParams): F[Either[List[CompletionItem], CompletionList]]
  def diagnostic(params: DocumentDiagnosticParams): F[DocumentDiagnosticReport]
  def codeLens(params: CodeLensParams): F[List[CodeLens]]

  def didChangeWatchedFiles(
    params: DidChangeWatchedFilesParams
  ): F[Unit]

  def executeCommand(params: ExecuteCommandParams): F[Unit]
  def shutdown(): F[Unit]
  def exit(): F[Unit]
}

object LanguageServer {

  def notAvailable[F[_]: MonadThrow]: LanguageServer[F] = defer(
    new Throwable("Server not available").raiseError[F, LanguageServer[F]]
  )

  def instance[F[_]: Async: TextDocumentManager: LanguageClient: ServerReload](
    dsi: DynamicSchemaIndex,
    runner: Runner.Optional[F],
  )(
    implicit sup: Supervisor[F]
  ): LanguageServer[F] =
    new LanguageServer[F] {

      private val iorToF: IorThrow ~> F =
        new (IorThrow ~> F) {
          def apply[A](fa: IorThrow[A]): F[A] = fa.toEither.liftTo[F]
        }

      val compiler = playground.Compiler.fromSchemaIndex(dsi)

      val completionProvider = CompletionProvider.forSchemaIndex(dsi)
      val diagnosticProvider = DiagnosticProvider.instance(compiler, runner)
      val lensProvider = CodeLensProvider.instance(compiler, runner)
      val commandProvider = CommandProvider.instance[F](compiler.mapK(iorToF), runner)

      def initialize(
        params: InitializeParams
      ): F[InitializeResult] = {
        val capabilities = new ServerCapabilities()
          .tap(_.setTextDocumentSync(TextDocumentSyncKind.Full))
          .tap(_.setDocumentFormattingProvider(true))
          .tap(_.setCompletionProvider(new CompletionOptions()))
          .tap(_.setDiagnosticProvider(new DiagnosticRegistrationOptions()))
          .tap(_.setCodeLensProvider(new CodeLensOptions()))

        new InitializeResult(capabilities).pure[F]
      }

      def initialized(params: InitializedParams): F[Unit] = Applicative[F].unit
      def shutdown(): F[Unit] = Applicative[F].unit

      def didChange(params: DidChangeTextDocumentParams): F[Unit] = {
        val changesAsList = params.getContentChanges.asScala.toList
        if (changesAsList.isEmpty)
          Applicative[F].unit
        else
          TextDocumentManager[F].put(
            params.getTextDocument().getUri(),
            changesAsList.head.getText(),
          )
      }

      def didOpen(params: DidOpenTextDocumentParams): F[Unit] = TextDocumentManager[F].put(
        params.getTextDocument().getUri(),
        params.getTextDocument().getText(),
      )

      def didSave(
        params: DidSaveTextDocumentParams
      ): F[Unit] = TextDocumentManager[F]
        .remove(params.getTextDocument().getUri())

      def didClose(
        params: DidCloseTextDocumentParams
      ): F[Unit] = TextDocumentManager[F].remove(params.getTextDocument().getUri())

      private val getFormatterWidth: F[Int] = LanguageClient[F]
        .configuration[Int]("smithyql.formatter.maxWidth")

      def formatting(
        params: DocumentFormattingParams
      ): F[List[TextEdit]] = TextDocumentProvider[F]
        .get(params.getTextDocument().getUri())
        .flatMap { text =>
          getFormatterWidth
            .map { maxWidth =>
              SmithyQLParser
                .parseFull(text)
                .map { parsed =>
                  val formatted = Formatter.format(parsed, maxWidth)

                  val lines = text.linesWithSeparators.toList

                  List(
                    new TextEdit(
                      new Range(
                        new Position(0, 0),
                        new Position(lines.indices.size, lines.last.size),
                      ),
                      formatted,
                    )
                  )
                }
                // doesn't parse, we won't format
                .getOrElse(Nil)
            }
        }

      def completion(
        position: CompletionParams
      ): F[Either[List[CompletionItem], CompletionList]] = TextDocumentManager[F]
        .get(position.getTextDocument().getUri())
        .map { documentText =>
          completionProvider
            .provide(
              documentText,
              converters.fromLSP.position(documentText, position.getPosition()),
            )
            .map(converters.toLSP.completionItem(documentText, _))
        }
        .map(Left(_))

      def diagnostic(
        params: DocumentDiagnosticParams
      ): F[DocumentDiagnosticReport] = TextDocumentManager[F]
        .get(params.getTextDocument().getUri())
        .map { documentText =>
          val diags = diagnosticProvider.getDiagnostics(
            params.getTextDocument().getUri(),
            documentText,
          )

          new DocumentDiagnosticReport(
            new RelatedFullDocumentDiagnosticReport(
              diags
                .map(converters.toLSP.diagnostic(documentText, _))
                .asJava
            )
          )
        }

      def codeLens(
        params: CodeLensParams
      ): F[List[CodeLens]] = TextDocumentManager[F].get(params.getTextDocument().getUri()).map {
        documentText =>
          lensProvider
            .provide(
              documentUri = params.getTextDocument.getUri(),
              documentText = documentText,
            )
            .map(converters.toLSP.codeLens(documentText, _))
      }

      def didChangeWatchedFiles(
        params: DidChangeWatchedFilesParams
      ): F[Unit] = ServerReload[F].prepare.flatMap {
        case prepared if !prepared.isChanged =>
          LanguageClient[F].showInfoMessage(
            "No change detected, not rebuilding server"
          )
        case prepared =>
          LanguageClient[F].showInfoMessage("Detected changes, will try to rebuild server...") *>
            ServerReload[F]
              .perform(prepared.params)
              .onError { case e =>
                LanguageClient[F].showErrorMessage(
                  "Couldn't reload server: " + e.getMessage
                )
              }
              .flatMap { stats =>
                // Can't make (and wait for) client requests while handling a client request (file change)
                {
                  LanguageClient[F].refreshDiagnostics *>
                    LanguageClient[F].refreshCodeLenses *> LanguageClient[F]
                      .showInfoMessage(
                        s"Reloaded Smithy Playground server with " +
                          s"${stats.importCount} imports, " +
                          s"${stats.dependencyCount} dependencies and " +
                          s"${stats.pluginCount} plugins"
                      )
                }.supervise(sup).void
              }
      }

      def executeCommand(
        params: ExecuteCommandParams
      ): F[Unit] = params
        .getArguments()
        .asScala
        .toList
        .traverse {
          case gson: JsonElement => converters.gsonToCirce(gson).as[String].liftTo[F]
          case s                 => new Throwable("Unsupported arg: " + s).raiseError[F, String]
        }
        .flatMap(commandProvider.runCommand(params.getCommand(), _))

      def exit(): F[Unit] = Applicative[F].unit
    }

  // Hoping https://github.com/typelevel/cats-tagless/issues/365 would simplify this
  implicit val suspendKLSP: SuspendK[LanguageServer] =
    new SuspendK[LanguageServer] {
      private val funk = Derive.functorK[LanguageServer]
      def mapK[F[_], G[_]](af: LanguageServer[F])(fk: F ~> G): LanguageServer[G] = funk.mapK(af)(fk)

      def suspend[F[_]]: LanguageServer[Suspended[LanguageServer, F, *]] =
        new LanguageServer[Suspended[LanguageServer, F, *]] {
          def initialize(params: InitializeParams): Suspended[LanguageServer, F, InitializeResult] =
            Suspended(_.initialize(params))

          def initialized(params: InitializedParams): Suspended[LanguageServer, F, Unit] =
            Suspended(_.initialized(params))

          def didChange(params: DidChangeTextDocumentParams): Suspended[LanguageServer, F, Unit] =
            Suspended(_.didChange(params))

          def didOpen(params: DidOpenTextDocumentParams): Suspended[LanguageServer, F, Unit] =
            Suspended(_.didOpen(params))

          def didSave(params: DidSaveTextDocumentParams): Suspended[LanguageServer, F, Unit] =
            Suspended(_.didSave(params))

          def didClose(params: DidCloseTextDocumentParams): Suspended[LanguageServer, F, Unit] =
            Suspended(_.didClose(params))

          def formatting(
            params: DocumentFormattingParams
          ): Suspended[LanguageServer, F, List[TextEdit]] = Suspended(_.formatting(params))

          def completion(
            position: CompletionParams
          ): Suspended[LanguageServer, F, Either[List[CompletionItem], CompletionList]] = Suspended(
            _.completion(position)
          )

          def diagnostic(
            params: DocumentDiagnosticParams
          ): Suspended[LanguageServer, F, DocumentDiagnosticReport] = Suspended(
            _.diagnostic(params)
          )

          def codeLens(params: CodeLensParams): Suspended[LanguageServer, F, List[CodeLens]] =
            Suspended(_.codeLens(params))

          def didChangeWatchedFiles(
            params: DidChangeWatchedFilesParams
          ): Suspended[LanguageServer, F, Unit] = Suspended(_.didChangeWatchedFiles(params))

          def executeCommand(params: ExecuteCommandParams): Suspended[LanguageServer, F, Unit] =
            Suspended(_.executeCommand(params))

          def shutdown(): Suspended[LanguageServer, F, Unit] = Suspended(_.shutdown())

          def exit(): Suspended[LanguageServer, F, Unit] = Suspended(_.exit())

        }

    }

  def defer[F[_]: FlatMap](
    fk: F[LanguageServer[F]]
  ): LanguageServer[F] = SuspendK[LanguageServer].deferKId(fk)

}
