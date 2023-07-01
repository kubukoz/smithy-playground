package playground.smithyutil

import smithy4s.schema.Schema
import smithy4s.schema.Schema.BijectionSchema
import smithy4s.schema.Schema.CollectionSchema
import smithy4s.schema.Schema.EnumerationSchema
import smithy4s.schema.Schema.LazySchema
import smithy4s.schema.Schema.MapSchema
import smithy4s.schema.Schema.NullableSchema
import smithy4s.schema.Schema.PrimitiveSchema
import smithy4s.schema.Schema.RefinementSchema
import smithy4s.schema.Schema.StructSchema
import smithy4s.schema.Schema.UnionSchema
import smithy4s.~>

/** Applies the underlying transformation on each node of the schema. */
final class TransitiveCompiler(
  underlying: Schema ~> Schema
) extends (Schema ~> Schema) {

  def apply[A](
    fa: Schema[A]
  ): Schema[A] =
    fa match {
      case e @ EnumerationSchema(_, _, _, _, _) => underlying(e)
      case p @ PrimitiveSchema(_, _, _)         => underlying(p)
      case u @ UnionSchema(_, _, _, _) =>
        underlying(u.copy(alternatives = u.alternatives.map(_.mapK(this))))
      case BijectionSchema(s, bijection)    => underlying(BijectionSchema(this(s), bijection))
      case LazySchema(suspend)              => underlying(LazySchema(suspend.map(this.apply)))
      case RefinementSchema(s, refinement)  => underlying(RefinementSchema(this(s), refinement))
      case c @ CollectionSchema(_, _, _, _) => underlying(c.copy(member = this(c.member)))
      case m @ MapSchema(_, _, _, _) => underlying(m.copy(key = this(m.key), value = this(m.value)))
      case s @ StructSchema(_, _, _, _) => underlying(s.copy(fields = s.fields.map(_.mapK(this))))
      case n @ NullableSchema(_)        => underlying(n.copy(underlying = this(n.underlying)))
    }

}
