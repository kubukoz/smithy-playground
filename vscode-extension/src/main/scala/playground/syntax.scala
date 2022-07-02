package playground

import cats.effect.IO
import typings.vscode.mod.CancellationToken
import scalajs.js._

object syntax {

  implicit class IOExtensions[T](io: IO[T]) {

    def withCancelToken(tok: CancellationToken): IO[T] = {

      val canc =
        IO.async_[Unit] { cb =>
          val _ = tok.onCancellationRequested { _ =>
            cb(Right(())): Any
          }
        } *> IO.canceled *> IO.never

      io.race(canc).map(_.merge)
    }

  }

}

/*
     "contributes": {
    "notebooks": [
      {
        "type": "smithyql",
        "displayName": "SmithyQL",
        "selector": [
          {
            "filenamePattern": "*.smithyql"
          }
        ]
      }
    ]
  }, */
// workspace.registerNotebookSerializer(
//   "smithyql",
//   NotebookSerializer(
//     deserializeNotebook =
//       (bytes, cancellation) => {
//         println(bytes)
//         println(42)
//         ???
//       },
//     serializeNotebook =
//       (nbd, cancellation) =>
//         // chan.appendLine("watter?")
//         {
//           println((nbd, cancellation))
//           ???
//         },
//   ),
// )
// register command
