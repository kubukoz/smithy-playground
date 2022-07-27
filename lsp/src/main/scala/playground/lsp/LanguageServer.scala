package playground.lsp

import cats.Applicative
import cats.MonadThrow
import cats.effect.kernel.Async
import cats.implicits._
import org.eclipse.lsp4j.ServerCapabilities
import org.eclipse.lsp4j.TextDocumentSyncKind
import org.eclipse.lsp4j._
import playground.CompletionProvider
import playground.smithyql.Formatter
import playground.smithyql.SmithyQLParser
import smithy4s.dynamic.DynamicSchemaIndex

import scala.jdk.CollectionConverters._
import scala.util.chaining._
import playground.Runner
import cats.effect.std
import org.http4s.client.Client
import org.http4s.Uri
import io.circe.Decoder
import cats.effect.kernel.Resource
import smithy4s.aws.AwsEnvironment
import playground.DiagnosticProvider
import playground.CodeLensProvider

trait LanguageServer[F[_]] {
  def initialize(params: InitializeParams): F[InitializeResult]
  def didChange(params: DidChangeTextDocumentParams): F[Unit]
  def didOpen(params: DidOpenTextDocumentParams): F[Unit]
  def didSave(params: DidSaveTextDocumentParams): F[Unit]
  def didClose(params: DidCloseTextDocumentParams): F[Unit]
  def formatting(params: DocumentFormattingParams): F[List[TextEdit]]
  def completion(position: CompletionParams): F[Either[List[CompletionItem], CompletionList]]
  def diagnostic(params: DocumentDiagnosticParams): F[DocumentDiagnosticReport]
  def codeLens(params: CodeLensParams): F[List[CodeLens]]
  def shutdown(): F[Unit]
  def exit(): F[Unit]
}

object LanguageServer {

  def instance[F[_]: Async: TextDocumentManager: LanguageClient: std.Console](
    dsi: DynamicSchemaIndex,
    log: String => F[Unit],
    client: Client[F],
    awsEnv: Resource[F, AwsEnvironment[F]],
  ): LanguageServer[F] =
    new LanguageServer[F] {

      private implicit val uriJsonDecoder: Decoder[Uri] = Decoder[String].emap(
        Uri.fromString(_).leftMap(_.message)
      )

      val compiler = playground.Compiler.fromSchemaIndex(dsi)

      val runner = Runner
        .forSchemaIndex[F](
          dsi,
          client,
          LanguageClient[F]
            .configuration(List(new ConfigurationItem().tap(_.setSection("smithyql.http.baseUrl"))))
            .flatMap(
              _.headOption
                .liftTo[F](new Throwable("InvalidConfigValueCount"))
                .flatMap(_.as[Uri].liftTo[F])
            ),
          awsEnv,
        )

      val completionProvider = CompletionProvider.forSchemaIndex(dsi)
      val diagnosticProvider = DiagnosticProvider.instance(compiler, runner)
      val lensProvider = CodeLensProvider.instance(compiler, runner)

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
        .configuration(
          new ConfigurationItem().tap(_.setSection("smithyql.formatter.maxWidth")) :: Nil
        )
        .flatMap {
          case width :: Nil => width.as[Int].liftTo[F]
          case r            => MonadThrow[F].raiseError[Int](new Throwable("config error: " + r))
        }

      def formatting(
        params: DocumentFormattingParams
      ): F[List[TextEdit]] = TextDocumentManager[F]
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
            .provide(documentText)
            .map(converters.toLSP.codeLens(documentText, _))
      }

      def exit(): F[Unit] = Applicative[F].unit
    }

}
