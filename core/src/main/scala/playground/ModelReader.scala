package playground

import cats.MonadThrow
import cats.implicits._
import smithy4s.dynamic.DynamicSchemaIndex
import software.amazon

object ModelReader {

  def buildSchemaIndex[F[_]: MonadThrow](
    model: amazon.smithy.model.Model
  ): F[DynamicSchemaIndex] = DynamicSchemaIndex.loadModel(model).liftTo[F]

}
