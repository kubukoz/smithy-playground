package playground.lsp

import cats.Applicative
import cats.effect.kernel.Async
import cats.implicits._
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
  def didChange(params: DidChangeTextDocumentParams): F[Unit]
  def didOpen(params: DidOpenTextDocumentParams): F[Unit]
  def didSave(params: DidSaveTextDocumentParams): F[Unit]
  def didClose(params: DidCloseTextDocumentParams): F[Unit]
  def formatting(params: DocumentFormattingParams): F[List[TextEdit]]
  def completion(position: CompletionParams): F[Either[List[CompletionItem], CompletionList]]
  def diagnostic(params: DocumentDiagnosticParams): F[DocumentDiagnosticReport]
  def codeLens(params: CodeLensParams): F[List[CodeLens]]
  def executeCommand(params: ExecuteCommandParams): F[Unit]
  def shutdown(): F[Unit]
  def exit(): F[Unit]
}

object LanguageServer {

  def instance[F[_]: Async: TextDocumentManager: LanguageClient](
    dsi: DynamicSchemaIndex,
    runner: Runner.Optional[F],
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

}
