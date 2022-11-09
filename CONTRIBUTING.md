# Contributing

Not much to be found here, but feel free to extend this document with anything useful.

## Infrastructure

There are two main infrastructure components:

- the language client: a VS Code extension using the `vscode-languageclient` library to start and communicate with a language server (using Coursier to fetch and run the jars)
- the langauge server: a Scala (JVM) application using the [`lsp4j`](https://github.com/eclipse/lsp4j) library to implement the LSP protocol.

The communication happens over standard I/O. Stdout (your `println` inside the server) is redirected to the logfile,`smithyql-log.txt` inside the workspace. Note: it might not work, so I suggest you write to `System.err` or use Main's `logOut` directly.

## Resources

- [lsp4j documentation](https://github.com/eclipse/lsp4j/blob/main/documentation/README.md)
