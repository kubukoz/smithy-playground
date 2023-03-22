package playground.smithyutil

import cats.implicits._
import smithy.api
import smithy4s.Refinement
import smithy4s.RefinementProvider
import smithy4s.Surjection
import smithy4s.schema.CollectionTag.IndexedSeqTag
import smithy4s.schema.CollectionTag.ListTag
import smithy4s.schema.CollectionTag.SetTag
import smithy4s.schema.CollectionTag.VectorTag
import smithy4s.schema.Primitive.PBigDecimal
import smithy4s.schema.Primitive.PBigInt
import smithy4s.schema.Primitive.PBlob
import smithy4s.schema.Primitive.PBoolean
import smithy4s.schema.Primitive.PByte
import smithy4s.schema.Primitive.PDocument
import smithy4s.schema.Primitive.PDouble
import smithy4s.schema.Primitive.PFloat
import smithy4s.schema.Primitive.PInt
import smithy4s.schema.Primitive.PLong
import smithy4s.schema.Primitive.PShort
import smithy4s.schema.Primitive.PString
import smithy4s.schema.Primitive.PTimestamp
import smithy4s.schema.Primitive.PUUID
import smithy4s.schema.Primitive.PUnit
import smithy4s.schema.Schema
import smithy4s.schema.Schema.CollectionSchema
import smithy4s.schema.Schema.MapSchema
import smithy4s.schema.Schema.PrimitiveSchema
import smithy4s.~>

object AddDynamicRefinements extends (Schema ~> Schema) {

  private def void[C, A](
    underlying: RefinementProvider[C, A, _]
  ): RefinementProvider.Simple[C, A] =
    Refinement
      .drivenBy[C]
      .contextual[A, A](c => Surjection(v => underlying.make(c).apply(v).as(v), identity))(
        underlying.tag
      )

  private implicit class SchemaOps[A](
    schema: Schema[A]
  ) {

    def reifyHint[B](
      implicit rp: RefinementProvider[B, A, _]
    ): Schema[A] = schema.hints.get(rp.tag).fold(schema)(schema.validated(_)(void(rp)))

  }

  private def collection[C[_], A](
    schema: Schema.CollectionSchema[C, A]
  ): Schema[C[A]] =
    schema.tag match {
      case ListTag   => schema.reifyHint(RefinementProvider.iterableLengthConstraint[List, A])
      case VectorTag => schema.reifyHint(RefinementProvider.iterableLengthConstraint[Vector, A])
      case SetTag    => schema.reifyHint(RefinementProvider.iterableLengthConstraint[Set, A])
      case IndexedSeqTag =>
        schema.reifyHint(RefinementProvider.iterableLengthConstraint[IndexedSeq, A])
    }

  def apply[A](
    schema: Schema[A]
  ): Schema[A] =
    schema match {
      case PrimitiveSchema(_, _, tag) =>
        tag match {
          case PString     => schema.reifyHint[api.Length].reifyHint[api.Pattern]
          case PByte       => schema.reifyHint[api.Range]
          case PShort      => schema.reifyHint[api.Range]
          case PInt        => schema.reifyHint[api.Range]
          case PLong       => schema.reifyHint[api.Range]
          case PFloat      => schema.reifyHint[api.Range]
          case PDouble     => schema.reifyHint[api.Range]
          case PBigInt     => schema.reifyHint[api.Range]
          case PBigDecimal => schema.reifyHint[api.Range]
          case PBlob       => schema.reifyHint[api.Length]
          case PUnit | PTimestamp | PDocument | PBoolean | PUUID => schema
        }

      case c: CollectionSchema[_, _] => collection(c)
      case m: MapSchema[_, _]        => m.reifyHint[api.Length]
      case _                         => schema
    }

}
