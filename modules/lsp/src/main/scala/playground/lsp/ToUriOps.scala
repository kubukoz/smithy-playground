package playground.lsp

import org.eclipse.lsp4j.TextDocumentIdentifier
import playground.language.Uri
import org.eclipse.lsp4j.WorkspaceFolder
import org.eclipse.lsp4j.TextDocumentItem

object ToUriOps {

  implicit final class TextDocumentIdentifierToUriSyntax(tdi: TextDocumentIdentifier) {
    def toUri: Uri = Uri.fromUriString(tdi.getUri())
  }

  implicit final class TextDocumentItemToUriSyntax(tdi: TextDocumentItem) {
    def toUri: Uri = Uri.fromUriString(tdi.getUri())
  }

  implicit final class WorkspaceFolderToUriSyntax(wf: WorkspaceFolder) {
    def toUri: Uri = Uri.fromUriString(wf.getUri())
  }

}
