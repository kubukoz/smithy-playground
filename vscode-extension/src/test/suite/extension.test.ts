import assert = require("assert");
import path = require("path");

// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
import * as vscode from "vscode";

const rootPath = path.resolve(__dirname, "../../../fixture");

const SERVER_VERSION = process.env.SERVER_VERSION || "latest.integration";

const getDocUri = (p: string) => {
  return vscode.Uri.file(getDocPath(p));
};

const getDocPath = (p: string) => {
  return path.resolve(rootPath, p);
};

suite("Extension Test Suite", () => {
  test("Completions appear after some time", async () => {
    const docUri = getDocUri("example.smithyql");

    await vscode.workspace
      .getConfiguration("")
      .update("smithyql.server.version", SERVER_VERSION);

    const doc = await vscode.workspace.openTextDocument(docUri);

    await vscode.window.showTextDocument(doc);
    const completions = await getCompletionsWhileEmpty(docUri);

    const results = completions.items.map(
      (it) => (it.label as vscode.CompletionItemLabel).label
    );

    try {
      assert.equal(results.length, 2);
      assert.equal(results[0], "CurrentTimestamp");
      assert.equal(results[1], "NextUUID");
    } catch (e) {
      // showing actual completions
      console.log(JSON.stringify(completions.items.map((it) => it.label)));
      throw e;
    }
  });
});

async function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function getCompletionsWhileEmpty(
  docUri: vscode.Uri
): Promise<vscode.CompletionList<vscode.CompletionItem>> {
  while (true) {
    const completions = (await vscode.commands.executeCommand(
      "vscode.executeCompletionItemProvider",
      docUri,
      new vscode.Position(0, 0)
    )) as vscode.CompletionList;

    if (completions.items.length == 0) await sleep(500);
    else return completions;
  }
}
