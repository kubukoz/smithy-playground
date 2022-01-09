package playground

import cats.implicits._
import playground.smithyql.SmithyQLParser
import playground.smithyql.WithSource
import playground.smithyql.WithSource.StructThing
import smithy.api.Documentation
import smithy.api.Http
import smithy4s.Service
import typings.vscode.mod

import types._
import util.chaining._
import smithy.api.ExternalDocumentation

object completions {

  def complete(
    service: Service[Alg, Op]
  ): (mod.TextDocument, mod.Position) => List[mod.CompletionItem] = {
    val completeOperationName = service
      .endpoints
      .map { e =>
        new mod.CompletionItem(e.name + ": Foo => Bar", mod.CompletionItemKind.Function).tap(
          _.detail = List(
            e.hints.get(Http).map { http =>
              show"HTTP ${http.method.value} ${http.uri.value} "
            },
            e.hints.get(Documentation).map(_.value),
            e.hints.get(ExternalDocumentation).map(_.value).map {
              _.map { case (k, v) => show"""${k.value}: ${v.value}""" }.mkString("\n")
            },
          ).flatten
            .map(_ + " ") // workaround for concatenation in the shortened view
            .mkString("\n\n")
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
