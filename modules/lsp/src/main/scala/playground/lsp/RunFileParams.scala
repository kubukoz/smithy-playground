package playground.lsp

import playground.language.Uri

// Params for the smithyql/runQuery LSP extension operation
case class RunFileParams(uri: Uri)
