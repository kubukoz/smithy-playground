package playground

import playground.smithyql.SmithyQLParser
import playground.smithyql.WithSource
import playground.smithyql.WithSource.StructThing
import smithy.api.Documentation
import smithy4s.Service
import typings.vscode.mod

import types._
import util.chaining._

object completions {

  def complete(
    service: Service[Alg, Op]
  ): (mod.TextDocument, mod.Position) => List[mod.CompletionItem] = {
    val completeOperationName = service
      .endpoints
      .map { e =>
        new mod.CompletionItem(e.name + ": Foo => Bar", mod.CompletionItemKind.Function).tap(
          _.detail = e.hints.get(Documentation).map(_.value).getOrElse(""): String
        )
      }

    (doc, pos) =>
      SmithyQLParser.parseFull(doc.getText()) match {
        case Left(_) =>
          // we can try to deal with this later
          Nil
        case Right(q) =>
          val matchingNode = WithSource.atPosition(q)(adapters.fromVscodePosition(doc)(pos))

          matchingNode
            .toList
            .flatMap {
              case WithSource.OperationThing(_) => completeOperationName
              case StructThing(_)               =>
                // todo: very much hardcoded
                List("foo", "bar").map { key =>
                  new mod.CompletionItem(key, mod.CompletionItemKind.Field)
                    .tap(_.insertText = key + " = ")
                }

              case _ => Nil
            }
      }
  }

}
