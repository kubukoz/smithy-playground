import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

const x = new LanguageClient(
  "test",
  "test2",
  {
    command: "",
  } as ServerOptions,
  {} as LanguageClientOptions
);
