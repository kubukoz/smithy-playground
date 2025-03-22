package playground.lsp

import org.eclipse.lsp4j.TextDocumentIdentifier
import org.eclipse.lsp4j.TextDocumentItem
import org.eclipse.lsp4j.WorkspaceFolder
import playground.language.Uri

// todo: move to converters?
object ToUriOps {

  implicit final class TextDocumentIdentifierToUriSyntax(
    tdi: TextDocumentIdentifier
  ) {
    def toUri: Uri = Uri.fromUriString(tdi.getUri())
  }

  implicit final class TextDocumentItemToUriSyntax(
    tdi: TextDocumentItem
  ) {
    def toUri: Uri = Uri.fromUriString(tdi.getUri())
  }

  implicit final class WorkspaceFolderToUriSyntax(
    wf: WorkspaceFolder
  ) {
    def toUri: Uri = Uri.fromUriString(wf.getUri())
  }

}
