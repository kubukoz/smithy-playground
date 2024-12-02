package playground.smithyutil

import cats.syntax.all.*
import smithy.api
import smithy.api.Length
import smithy4s.Refinement
import smithy4s.RefinementProvider
import smithy4s.Surjection
import smithy4s.schema.Primitive.*
import smithy4s.schema.Schema
import smithy4s.schema.Schema.*
import smithy4s.~>

import RefinementProvider.given

/** Reifies refinement hints into the schema.
  *
  * Notably, this does NOT recurse! In order to traverse an entire schema recursively, this has to
  * be wrapped in transformTransitivelyK. This is done for separation of concerns.
  */
object AddDynamicRefinements extends (Schema ~> Schema) {

  private def void[C, A](
    underlying: RefinementProvider[C, A, ?]
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
  ): Schema[C[A]] = schema.reifyHint(
    using RefinementProvider.lengthConstraint(schema.tag.iterator(_).size)
  )

  private def enumSchema[A](
    schema: Schema.EnumerationSchema[A]
  ): Schema[A] = schema
    .reifyHint[Length](
      using RefinementProvider.lengthConstraint(schema.total(_).stringValue.size)
    )
    .reifyHint(
      using RefinementProvider.rangeConstraint[A, Int](schema.total(_).intValue)
    )
    .reifyHint(
      using RefinementProvider.patternConstraint(schema.total(_).stringValue)
    )

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
          case PByte       => (schema: Schema[Byte]).reifyHint[api.Range]
          case PShort      => (schema: Schema[Short]).reifyHint[api.Range]
          case PInt        => (schema: Schema[Int]).reifyHint[api.Range]
          case PLong       => (schema: Schema[Long]).reifyHint[api.Range]
          case PFloat      => (schema: Schema[Float]).reifyHint[api.Range]
          case PDouble     => (schema: Schema[Double]).reifyHint[api.Range]
          case PBigInt     => (schema: Schema[BigInt]).reifyHint[api.Range]
          case PBigDecimal => (schema: Schema[BigDecimal]).reifyHint[api.Range]
          case PBlob       => schema.reifyHint[api.Length]
          case PTimestamp | PDocument | PBoolean | PUUID => schema
        }

      case c: CollectionSchema[a, f] => collection[a, f](c)
      case m: MapSchema[_, _]        => m.reifyHint[api.Length]
      case e: EnumerationSchema[_]   => enumSchema(e)
      // explicitly handling each remaining case, in order to get a "missing match" warning if the schema model changes
      case b: BijectionSchema[_, _]  => b
      case r: RefinementSchema[_, _] => r
      case s: StructSchema[_]        => s
      case l: LazySchema[_]          => l
      case u: UnionSchema[_]         => u
      case n: OptionSchema[_]        => n
    }

}
