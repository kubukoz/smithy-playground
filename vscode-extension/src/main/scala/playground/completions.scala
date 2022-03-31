package playground

import cats.implicits._
import playground.smithyql.SmithyQLParser
import playground.smithyql.WithSource
import smithy.api.Documentation
import smithy.api.ExternalDocumentation
import smithy.api.Http
import smithy4s.Service
import typings.vscode.mod

import smithyql.CompletionSchematic
import util.chaining._
import playground.smithyql.CompletionItem.Field
import playground.smithyql.CompletionItem.UnionMember
import scala.scalajs.js.JSConverters._

object completions {

  def complete[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    service: Service[Alg, Op]
  ): (mod.TextDocument, mod.Position) => List[mod.CompletionItem] = {
    val completeOperationName = service
      .endpoints
      .map { e =>
        val getName = GetNameHint.singleton
        new mod.CompletionItem(
          s"${e.name}",
          mod.CompletionItemKind.Function,
        )
          .tap(_.insertText = e.name)
          .tap(_.detail =
            s"${e.input.compile(getName).get.value} => ${e.output.compile(getName).get.value}"
          )
          .tap(
            _.documentation = List(
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
              case WithSource.OperationContext(_) => completeOperationName
              case WithSource.InputContext(ctx) =>
                val e = service.endpoints.find(_.name == q.operationName.value.text).get
                // todo caching
                val result = e.input.compile(new CompletionSchematic).apply(ctx)

                result.map { key =>
                  key match {
                    case Field(label) =>
                      new mod.CompletionItem(label, mod.CompletionItemKind.Field)
                        // todo determine RHS based on field type
                        .tap(_.insertText = s"$label = ")

                    case UnionMember(label, deprecated) =>
                      new mod.CompletionItem(label, mod.CompletionItemKind.Class)
                        .tap(_.insertText = new mod.SnippetString(s"$label = {$$0},"))
                        .tap(item =>
                          if (deprecated)
                            item.tags =
                              List[mod.CompletionItemTag](
                                mod.CompletionItemTag.Deprecated
                              ).toJSArray
                          else
                            Nil
                        )
                  }
                }

              case _ => Nil
            }
      }
  }

}
