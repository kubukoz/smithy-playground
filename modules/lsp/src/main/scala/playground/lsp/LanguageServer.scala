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
import com.google.gson.JsonElement
import com.google.gson.JsonPrimitive
import org.eclipse.lsp4j
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

// todo: independentize this from lsp4j and move to kernel
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
    params: lsp4j.DocumentFormattingParams
  ): F[List[lsp4j.TextEdit]]

  def completion(
    position: lsp4j.CompletionParams
  ): F[Either[List[lsp4j.CompletionItem], lsp4j.CompletionList]]

  def diagnostic(
    params: lsp4j.DocumentDiagnosticParams
  ): F[lsp4j.DocumentDiagnosticReport]

  def codeLens(
    params: lsp4j.CodeLensParams
  ): F[List[lsp4j.CodeLens]]

  def documentSymbol(
    params: lsp4j.DocumentSymbolParams
  ): F[List[lsp4j.DocumentSymbol]]

  def didChangeWatchedFiles(
    params: lsp4j.DidChangeWatchedFilesParams
  ): F[Unit]

  def executeCommand(
    params: lsp4j.ExecuteCommandParams
  ): F[Unit]

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

      def initialize[A](workspaceFolders: List[Uri]): F[InitializeResult] = {
        def capabilities(compiler: ServerCapabilitiesCompiler): compiler.Result = {
          given Semigroup[compiler.Result] = compiler.semigroup
          compiler.textDocumentSync(TextDocumentSyncKind.Full) |+|
            compiler.documentFormattingProvider |+|
            compiler.completionProvider |+|
            compiler.diagnosticProvider |+|
            compiler.codeLensProvider |+|
            compiler.documentSymbolProvider
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
            .onError { case e => LanguageClient[F].showErrorMessage(e.getMessage()) }
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
        params: lsp4j.DocumentFormattingParams
      ): F[List[lsp4j.TextEdit]] = {
        val uri = converters.fromLSP.uri(params.getTextDocument())

        TextDocumentProvider[F].get(uri).flatMap { doc =>
          formattingProvider(uri).map(_.map(converters.toLSP.textEdit(_, LocationMap(doc))))
        }
      }

      def completion(
        position: lsp4j.CompletionParams
      ): F[Either[List[lsp4j.CompletionItem], lsp4j.CompletionList]] = TextDocumentManager[F]
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
        params: lsp4j.DocumentDiagnosticParams
      ): F[lsp4j.DocumentDiagnosticReport] = {
        val documentUri = converters.fromLSP.uri(params.getTextDocument())
        TextDocumentManager[F]
          .get(documentUri)
          .map { documentText =>
            val diags = diagnosticProvider.getDiagnostics(
              params.getTextDocument().getUri(),
              documentText,
            )

            val map = LocationMap(documentText)

            new lsp4j.DocumentDiagnosticReport(
              new lsp4j.RelatedFullDocumentDiagnosticReport(
                diags
                  .map(converters.toLSP.diagnostic(map, _))
                  .asJava
              )
            )
          }
      }

      def codeLens(
        params: lsp4j.CodeLensParams
      ): F[List[lsp4j.CodeLens]] = TextDocumentManager[F]
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
        params: lsp4j.DocumentSymbolParams
      ): F[List[lsp4j.DocumentSymbol]] = TextDocumentManager[F]
        .get(converters.fromLSP.uri(params.getTextDocument()))
        .map { text =>
          val map = LocationMap(text)

          DocumentSymbolProvider.make(text).map(converters.toLSP.documentSymbol(map, _))
        }

      def didChangeWatchedFiles(
        params: lsp4j.DidChangeWatchedFilesParams
      ): F[Unit] = ServerLoader[F]
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
        params: lsp4j.ExecuteCommandParams
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
        new lsp4j.ExecuteCommandParams()
          .tap(_.setCommand(playground.language.Command.RUN_FILE))
          .tap(_.setArguments(List(new JsonPrimitive(params.uri.value): Object).asJava))
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
}

case class ServerInfo(name: String, version: String)

case class InitializeResult(
  serverCapabilities: (compiler: ServerCapabilitiesCompiler) => compiler.Result,
  serverInfo: ServerInfo,
)
