package playground.smithyutil

import cats.implicits._
import smithy.api
import smithy4s.Refinement
import smithy4s.RefinementProvider
import smithy4s.Surjection
import smithy4s.schema.CollectionTag._
import smithy4s.schema.Primitive._
import smithy4s.schema.Schema
import smithy4s.schema.Schema._
import smithy4s.~>

import RefinementProvider.given

/** Reifies refinement hints into the schema.
  *
  * Notably, this does NOT recurse! In order to traverse an entire schema recursively, this has to
  * be wrapped in TransitiveCompiler. This is done for separation of concerns.
  */
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
      using rp: RefinementProvider.Simple[B, A]
    ): Schema[A] = schema.hints.get(rp.tag).fold(schema)(schema.validated(_)(void(rp)))

  }

  private def collection[C[_], A](
    schema: Schema.CollectionSchema[C, A]
  ): Schema[C[A]] =
    schema.tag match {
      case ListTag       => schema.reifyHint[api.Length]
      case VectorTag     => schema.reifyHint[api.Length]
      case SetTag        => schema.reifyHint[api.Length]
      case IndexedSeqTag => schema.reifyHint[api.Length]
    }

  def apply[A](
    schema: Schema[A]
  ): Schema[A] =
    schema match {
      case p: PrimitiveSchema[?] =>
        p.tag match {
          case PString =>
            schema
              .reifyHint[api.Length]
              .reifyHint[api.Pattern]
          case PByte =>
            schema.reifyHint[api.Range](
              using RefinementProvider.numericRangeConstraints[Byte]
            )
          case PShort =>
            schema.reifyHint[api.Range](
              using RefinementProvider.numericRangeConstraints[Short]
            )
          case PInt =>
            schema.reifyHint[api.Range](
              using RefinementProvider.numericRangeConstraints[Int]
            )
          case PLong =>
            schema.reifyHint[api.Range](
              using RefinementProvider.numericRangeConstraints[Long]
            )
          case PFloat =>
            schema.reifyHint[api.Range](
              using RefinementProvider.numericRangeConstraints[Float]
            )
          case PDouble =>
            schema.reifyHint[api.Range](
              using RefinementProvider.numericRangeConstraints[Double]
            )
          case PBigInt =>
            schema.reifyHint[api.Range](
              using RefinementProvider.numericRangeConstraints[BigInt]
            )
          case PBigDecimal =>
            schema.reifyHint[api.Range](
              using RefinementProvider.numericRangeConstraints[BigDecimal]
            )
          case PBlob                                     => schema.reifyHint[api.Length]
          case PTimestamp | PDocument | PBoolean | PUUID => schema
        }

      case c @ CollectionSchema(_, _, _, _) => collection(c)
      case m: MapSchema[_, _]               => m.reifyHint[api.Length]
      // explicitly handling each remaining case, in order to get a "mising match" warning if the schema model changes
      case b: BijectionSchema[_, _]  => b
      case r: RefinementSchema[_, _] => r
      case e: EnumerationSchema[_]   => e
      case s: StructSchema[_]        => s
      case l: LazySchema[_]          => l
      case u: UnionSchema[_]         => u
      case n: OptionSchema[_]        => n
    }

}
