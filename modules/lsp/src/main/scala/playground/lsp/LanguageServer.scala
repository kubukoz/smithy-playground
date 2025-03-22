package playground.lsp

import cats.Applicative
import cats.FlatMap
import cats.MonadThrow
import cats.data.Kleisli
import cats.effect.kernel.Async
import cats.parse.LocationMap
import cats.syntax.all.*
import cats.tagless.Derive
import cats.tagless.FunctorK
import cats.tagless.implicits.*
import cats.~>
import com.google.gson.JsonElement
import com.google.gson.JsonPrimitive
import org.eclipse.lsp4j.*
import playground.CompilationError
import playground.CompilationFailed
import playground.FileCompiler
import playground.FileRunner
import playground.OperationCompiler
import playground.PreludeCompiler
import playground.ServiceIndex
import playground.language
import playground.language.CodeLensProvider
import playground.language.CommandProvider
import playground.language.CommandResultReporter
import playground.language.CompletionProvider
import playground.language.DiagnosticProvider
import playground.language.DocumentSymbolProvider
import playground.language.Feedback
import playground.language.FormattingProvider
import playground.language.TextDocumentProvider
import playground.language.Uri
import playground.lsp.buildinfo.BuildInfo
import playground.types.*
import smithy4s.dynamic.DynamicSchemaIndex

import scala.jdk.CollectionConverters.*
import scala.util.chaining.*

trait LanguageServer[F[_]] {

  def initialize(
    params: InitializeParams
  ): F[InitializeResult]

  def initialized(
    params: InitializedParams
  ): F[Unit]

  def didChange(
    params: DidChangeTextDocumentParams
  ): F[Unit]

  def didOpen(
    params: DidOpenTextDocumentParams
  ): F[Unit]

  def didSave(
    params: DidSaveTextDocumentParams
  ): F[Unit]

  def didClose(
    params: DidCloseTextDocumentParams
  ): F[Unit]

  def formatting(
    params: DocumentFormattingParams
  ): F[List[TextEdit]]

  def completion(
    position: CompletionParams
  ): F[Either[List[CompletionItem], CompletionList]]

  def diagnostic(
    params: DocumentDiagnosticParams
  ): F[DocumentDiagnosticReport]

  def codeLens(
    params: CodeLensParams
  ): F[List[CodeLens]]

  def documentSymbol(
    params: DocumentSymbolParams
  ): F[List[DocumentSymbol]]

  def didChangeWatchedFiles(
    params: DidChangeWatchedFilesParams
  ): F[Unit]

  def executeCommand(
    params: ExecuteCommandParams
  ): F[Unit]

  def runFile(
    params: RunFileParams
  ): F[Unit]

  def shutdown: F[Unit]
  def exit: F[Unit]
}

object LanguageServer {

  def notAvailable[F[_]: MonadThrow]: LanguageServer[F] = defer(
    new Throwable("Server not available").raiseError[F, LanguageServer[F]]
  )

  def instance[
    F[_]: Async: TextDocumentManager: LanguageClient: ServerLoader: CommandResultReporter
  ](
    dsi: DynamicSchemaIndex,
    runner: FileRunner.Resolver[F],
  ): LanguageServer[F] =
    new LanguageServer[F] {

      private val iorToF: IorThrow ~> F =
        new (IorThrow ~> F) {

          def apply[A](
            fa: IorThrow[A]
          ): F[A] = fa.toEither.liftTo[F]

        }

      // see if we can pass this everywhere
      // https://github.com/kubukoz/smithy-playground/issues/164
      val serviceIndex: ServiceIndex = ServiceIndex.fromServices(dsi.allServices.toList)

      val compiler: FileCompiler[IorThrow] = FileCompiler
        .instance(
          PreludeCompiler.instance[CompilationError.InIorNel](serviceIndex),
          OperationCompiler.fromSchemaIndex(dsi),
        )
        .mapK(CompilationFailed.wrapK)

      val completionProvider: CompletionProvider = CompletionProvider.forSchemaIndex(dsi)
      val diagnosticProvider: DiagnosticProvider[F] = DiagnosticProvider.instance(compiler, runner)
      val lensProvider: CodeLensProvider[F] = CodeLensProvider.instance(compiler, runner)

      val commandProvider: CommandProvider[F] = CommandProvider
        .instance[F](compiler.mapK(iorToF), runner)

      private val getFormatterWidth: F[Int] = LanguageClient[F]
        .configuration(ConfigurationValue.maxWidth)

      val formattingProvider: Uri => F[List[language.TextEdit]] = FormattingProvider.provider(
        getFormatterWidth
      )

      def initialize(
        params: InitializeParams
      ): F[InitializeResult] = {
        val capabilities = new ServerCapabilities()
          .tap(_.setTextDocumentSync(TextDocumentSyncKind.Full))
          .tap(_.setDocumentFormattingProvider(true))
          .tap(_.setCompletionProvider(new CompletionOptions()))
          .tap(_.setDiagnosticProvider(new DiagnosticRegistrationOptions()))
          .tap(_.setCodeLensProvider(new CodeLensOptions()))
          .tap(_.setDocumentSymbolProvider(true))

        val serverInfo = new ServerInfo("Smithy Playground", BuildInfo.version)

        val wsf = params
          .getWorkspaceFolders()
          .asScala
          .toList
          .map(converters.fromLSP.uri(_))

        Feedback[F]
          .showInfoMessage(
            s"Hello from Smithy Playground v${BuildInfo.version}! Loading project..."
          ) *>
          ServerLoader[F]
            .prepare(wsf.some)
            .flatMap { prepped =>
              ServerLoader[F].perform(prepped.params).flatTap { stats =>
                Feedback[F]
                  .showInfoMessage(
                    s"Loaded Smithy Playground server with ${stats.render}"
                  )
              }
            }
            .onError { case e => LanguageClient[F].showErrorMessage(e.getMessage()) }
            .attempt
            .as(new InitializeResult(capabilities, serverInfo))
      }

      def initialized(
        params: InitializedParams
      ): F[Unit] = Applicative[F].unit

      def shutdown: F[Unit] = Applicative[F].unit

      def didChange(
        params: DidChangeTextDocumentParams
      ): F[Unit] = {
        val changesAsList = params.getContentChanges.asScala.toList
        if (changesAsList.isEmpty)
          Applicative[F].unit
        else
          TextDocumentManager[F].put(
            converters.fromLSP.uri(params.getTextDocument()),
            changesAsList.head.getText(),
          )
      }

      def didOpen(
        params: DidOpenTextDocumentParams
      ): F[Unit] = TextDocumentManager[F].put(
        converters.fromLSP.uri(params.getTextDocument()),
        params.getTextDocument().getText(),
      )

      def didSave(
        params: DidSaveTextDocumentParams
      ): F[Unit] = TextDocumentManager[F]
        .remove(converters.fromLSP.uri(params.getTextDocument()))

      def didClose(
        params: DidCloseTextDocumentParams
      ): F[Unit] = TextDocumentManager[F].remove(
        converters.fromLSP.uri(params.getTextDocument())
      )

      def formatting(
        params: DocumentFormattingParams
      ): F[List[TextEdit]] = {
        val uri = converters.fromLSP.uri(params.getTextDocument())

        TextDocumentProvider[F].get(uri).flatMap { doc =>
          formattingProvider(uri).map(_.map(converters.toLSP.textEdit(_, LocationMap(doc))))
        }
      }

      def completion(
        position: CompletionParams
      ): F[Either[List[CompletionItem], CompletionList]] = TextDocumentManager[F]
        .get(converters.fromLSP.uri(position.getTextDocument))
        .map { documentText =>
          val map = LocationMap(documentText)

          completionProvider
            .provide(
              documentText,
              converters.fromLSP.position(map, position.getPosition()),
            )
            .map(converters.toLSP.completionItem(map, _))
        }
        .map(Left(_))

      def diagnostic(
        params: DocumentDiagnosticParams
      ): F[DocumentDiagnosticReport] = {
        val documentUri = converters.fromLSP.uri(params.getTextDocument())
        TextDocumentManager[F]
          .get(documentUri)
          .map { documentText =>
            val diags = diagnosticProvider.getDiagnostics(
              params.getTextDocument().getUri(),
              documentText,
            )

            val map = LocationMap(documentText)

            new DocumentDiagnosticReport(
              new RelatedFullDocumentDiagnosticReport(
                diags
                  .map(converters.toLSP.diagnostic(map, _))
                  .asJava
              )
            )
          }
      }

      def codeLens(
        params: CodeLensParams
      ): F[List[CodeLens]] = TextDocumentManager[F]
        .get(converters.fromLSP.uri(params.getTextDocument()))
        .map { documentText =>
          val map = LocationMap(documentText)

          lensProvider
            .provide(
              documentUri = converters.fromLSP.uri(params.getTextDocument),
              documentText = documentText,
            )
            .map(converters.toLSP.codeLens(map, _))
        }

      def documentSymbol(
        params: DocumentSymbolParams
      ): F[List[DocumentSymbol]] = TextDocumentManager[F]
        .get(converters.fromLSP.uri(params.getTextDocument()))
        .map { text =>
          val map = LocationMap(text)

          DocumentSymbolProvider.make(text).map(converters.toLSP.documentSymbol(map, _))
        }

      def didChangeWatchedFiles(
        params: DidChangeWatchedFilesParams
      ): F[Unit] = ServerLoader[F]
        .prepare(workspaceFolders = None)
        .flatMap {
          case prepared if !prepared.isChanged =>
            Feedback[F].showInfoMessage(
              LanguageClient.NoChangeDetected
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

      def runFile(
        params: RunFileParams
      ): F[Unit] = executeCommand(
        new ExecuteCommandParams()
          .tap(_.setCommand(playground.language.Command.RUN_FILE))
          .tap(_.setArguments(List(new JsonPrimitive(params.uri.value): Object).asJava))
      )

      def exit: F[Unit] = Applicative[F].unit
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
