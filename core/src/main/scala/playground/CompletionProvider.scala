package playground

import playground.smithyql.CompletionItem
import playground.smithyql.OperationName
import playground.smithyql.Position
import playground.smithyql.SmithyQLParser
import playground.smithyql.WithSource
import smithy4s.Service

import smithyql.CompletionSchematic

object CompletionProvider {

  def make[Alg[_[_, _, _, _, _]], Op[_, _, _, _, _]](
    service: Service[Alg, Op]
  ): (String, Position) => List[CompletionItem] = {
    val completeOperationName = service
      .endpoints
      .map(CompletionItem.forOperation)

    val completionsByEndpoint: Map[OperationName, CompletionSchematic.ResultR[Any]] =
      service
        .endpoints
        .map { endpoint =>
          OperationName(endpoint.name) -> endpoint.input.compile(new CompletionSchematic).get
        }
        .toMap

    (doc, pos) =>
      SmithyQLParser.parseFull(doc) match {
        case Left(_) if doc.isBlank() => completeOperationName
        case Left(_)                  =>
          // we can try to deal with this later
          Nil

        case Right(q) =>
          val matchingNode = WithSource.atPosition(q)(pos)

          matchingNode
            .toList
            .flatMap {
              case WithSource.NodeContext.OperationContext(_) => completeOperationName
              case WithSource.NodeContext.InputContext(ctx) =>
                completionsByEndpoint(q.operationName.value)
                  .apply(ctx)
            }
      }
  }

}
