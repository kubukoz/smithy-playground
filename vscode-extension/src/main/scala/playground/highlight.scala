package playground

import typings.vscode.mod

import scala.scalajs.js.JSConverters._
import scala.util.chaining._

import types._

object highlight {

  def getHighlights[F[_]](
    doc: mod.TextDocument,
    c: Compiler[IorThrow],
    runner: Runner.Optional[F],
  ): List[mod.Diagnostic] = DiagnosticProvider
    .instance(c, runner)
    .getDiagnostics(doc.fileName, doc.getText())
    .map(translateDiagnostic(doc, _))

  private def translateDiagnostic(doc: mod.TextDocument, diag: CompilationError): mod.Diagnostic =
    new mod.Diagnostic(
      adapters.toVscodeRange(doc, diag.range),
      diag.err.render,
      diag.severity match {
        case DiagnosticSeverity.Error       => mod.DiagnosticSeverity.Error
        case DiagnosticSeverity.Warning     => mod.DiagnosticSeverity.Warning
        case DiagnosticSeverity.Information => mod.DiagnosticSeverity.Information
      },
    ).tap(_.tags =
      diag
        .tags
        .map[mod.DiagnosticTag] {
          case DiagnosticTag.Deprecated => mod.DiagnosticTag.Deprecated
          case DiagnosticTag.Unused     => mod.DiagnosticTag.Unnecessary
        }
        .toJSArray
    )

}
