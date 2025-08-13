import { commands, ExtensionContext, window, workspace } from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { buildArgs, CoursierCall, withDebug, withTracer } from "./coursier";

export function activate(context: ExtensionContext) {
  const serverArtifact = workspace
    .getConfiguration()
    .get<string>("smithyql.server.artifact");

  const serverVersion = workspace
    .getConfiguration()
    .get<string>("smithyql.server.version");

  const coursierTTL = serverVersion === "latest.integration" ? "0" : "1h";

  const outputChannel = window.createOutputChannel(
    "Smithy Playground",
    "smithyql"
  );

  const enableTracer = workspace
    .getConfiguration()
    .get<boolean>("smithyql.server.trace");

  const enableDebug = workspace
    .getConfiguration()
    .get<boolean>("smithyql.server.debug");

  const base: CoursierCall = {
    coursierArgs: ["--ttl", coursierTTL],
    app: {
      maven: { artifact: serverArtifact, version: serverVersion },
      args: [],
    },
  };

  const lspClient = new LanguageClient(
    "smithyPlayground",
    "Smithy Playground",
    {
      command: "cs",
      args: buildArgs(
        withTracer(enableTracer)(
          //
          withDebug(enableDebug)(base)
          //
        )
      ),
    },
    {
      documentSelector: [{ language: "smithyql" }],
      synchronize: {
        fileEvents: workspace.createFileSystemWatcher(
          "**/{build/smithy-dependencies.json,.smithy.json,smithy-build.json}"
        ),
      },
      outputChannel,
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

  const registerRestartCommand = commands.registerCommand(
    "smithyql.restart",
    () => {
      lspClient.restart();
    }
  );

  const registerOutputPanelNotification = lspClient.onNotification(
    "smithyql/showOutputPanel",
    (_) => lspClient.outputChannel.show(true)
  );

  lspClient.start();

  context.subscriptions.push(
    lspClient,
    registerRunCommand,
    registerRestartCommand,
    registerOutputPanelNotification
  );
}
