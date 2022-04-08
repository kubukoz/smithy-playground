package playground.server;

import org.eclipse.lsp4j.services.*;
import org.eclipse.lsp4j.*;
import org.eclipse.lsp4j.launch.*;
import java.util.concurrent.CompletableFuture;

public class Main {

	public static void main(String[] args) throws Exception {
		var launcher = LSPLauncher.createServerLauncher(new A(), System.in, System.out);
		launcher.startListening().get();
	}
}

class A implements LanguageServer {
	public WorkspaceService getWorkspaceService() {
		return null;
	}

	public TextDocumentService getTextDocumentService() {
		return null;
	}

	public void exit() {

	}

	public CompletableFuture<Object> shutdown() {
		return null;
	}

	public CompletableFuture<InitializeResult> initialize(InitializeParams params) {
		return null;
	}
}
// class A implements LanguageServer {

// }
// class A(implicit rt:IORuntime)extends LanguageServer
// {

// def initialize(
// x: InitializeParams
// ): CompletableFuture[InitializeResult] = CompletableFutures.computeAsync(()
// =>
// new InitializeResult()
// )

// def shutdown(): CompletableFuture[Object] = ???

// def exit(): Unit = ???

// def getTextDocumentService(): TextDocumentService = ???

// def getWorkspaceService(): WorkspaceService = ???

// }
