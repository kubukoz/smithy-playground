# Contributing

Not much to be found here, but feel free to extend this document with anything useful.

## Infrastructure

There are two main infrastructure components:

- the language client: a VS Code extension using the `vscode-languageclient` library to start and communicate with a language server (using Coursier to fetch and run the jars)
- the langauge server: a Scala (JVM) application using the [`lsp4j`](https://github.com/eclipse/lsp4j) library to implement the LSP protocol.

The communication happens over standard I/O. Stdout (your `println` inside the server) is redirected to the logfile,`smithyql-log.txt` inside the workspace. Note: it might not work, so I suggest you write to `System.err` or use Main's `logOut` directly.

## Testing

All commands here assume you're in sbt (unless it's anything related to vscode).
Bloop also works, but some changes might require to re-run codegen (so, `bloopInstall` or at least `managedSources`) before you re-run the bloop.

### Unit tests

Normal, plain tests: you'll find these in the `test` directories of each module in [`modules/`](./modules), and you can run them with `test`.
This actually currently includes the integration tests (read on for more detail).

### Integration tests

These aren't very resource heavy (or slow), so they run as part of the normal `test` configuration.
You'll find these in [`modules/lsp/src/test`](modules/lsp/src/test), and they run on almost the highest abstraction level of the LSP server: by calling an almost-real instance of `LanguageServer`.

This doesn't exercise the standard I/O interface of the server, but it's still a decent way to test the composition of all the modules with a filesystem directory ([`modules/lsp/src/test/resources/test-workspace`](modules/lsp/src/test/resources/test-workspace)).

To run integration tests, run `lsp/test`.

## Extension integration tests

This is as close as we get to e2e tests: this uses a real instance of VS Code to open a `.smithyql` file, and waits for completions to show up.
The tests are configured to run against the latest snapshot version of the LSP server, in CI this means the current branch's version.

There are two ways to run these locally:

### Outside VS Code

This may not work if you already have a VS Code window open: the following command will fetch a Code instance from the Internet and launch it, then run the test suite against it:

```bash
cd vscode-extension && yarn test
```

The version of Code used for this is hardcoded in [`runTest.ts`](./vscode-extension/src/test/runTest.ts).

### In VS Code

If you already have a VS Code instance open , you can use the "Run Extension Tests" launch configuration in [`.vscode/launch.json`](.vscode/launch.json). It may be helpful to run `yarn watch` so that your TypeScript changes get rebuilt in watch mode.

## Resources

- [lsp4j documentation](https://github.com/eclipse/lsp4j/blob/main/documentation/README.md)
