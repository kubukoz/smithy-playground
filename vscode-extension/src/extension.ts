import { commands, ExtensionContext, workspace } from "vscode";
import {
  LanguageClientOptions,
  LanguageClient,
} from "vscode-languageclient/node";

export function activate(context: ExtensionContext) {
  const serverArtifact = workspace
    .getConfiguration()
    .get<string>("smithyql.server.artifact");

  const serverVersion = workspace
    .getConfiguration()
    .get<string>("smithyql.server.version");

  const lspClient = new LanguageClient(
    "smithyPlayground",
    "Smithy Playground",
    {
      command: "cs",
      args: [
        "launch",
        `${serverArtifact}:${serverVersion}`,
        // "--",
        // "-J-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,quiet=y,address=5005",
      ],
    },
    {
      documentSelector: [{ language: "smithyql" }],
    }
  );
  const registerRunCommand = commands.registerTextEditorCommand(
    "smithyql.runQuery",
    (editor) => {
      lspClient.sendRequest("smithyql/runQuery", {
        uri: editor.document.uri.toString(),
      });
    }
  );

  const registerOutputPanelNotification = lspClient.onNotification(
    "smithyql/showOutputPanel",
    (_) => lspClient.outputChannel.show(true)
  );

  lspClient.start();

  context.subscriptions.push(
    {
      dispose() {
        lspClient.stop();
      },
    },
    registerRunCommand,
    registerOutputPanelNotification
  );
}
