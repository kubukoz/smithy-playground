package playground.smithyutil

import smithy4s.schema.Schema
import smithy4s.schema.Schema.BijectionSchema
import smithy4s.schema.Schema.CollectionSchema
import smithy4s.schema.Schema.EnumerationSchema
import smithy4s.schema.Schema.LazySchema
import smithy4s.schema.Schema.MapSchema
import smithy4s.schema.Schema.PrimitiveSchema
import smithy4s.schema.Schema.RefinementSchema
import smithy4s.schema.Schema.StructSchema
import smithy4s.schema.Schema.UnionSchema
import smithy4s.~>

// Applies the underlying transformation on each node of the schema that has its own hints
final class TransitiveCompiler(underlying: Schema ~> Schema) extends (Schema ~> Schema) {

  def apply[A](fa: Schema[A]): Schema[A] =
    fa match {
      case e @ EnumerationSchema(_, _, _, _) => underlying(e)
      case p @ PrimitiveSchema(_, _, _)      => underlying(p)
      case u @ UnionSchema(_, _, _, _) =>
        underlying(u.copy(alternatives = u.alternatives.map(_.mapK(this))))
      case BijectionSchema(s, bijection) => BijectionSchema(this(s), bijection)
      case LazySchema(suspend)           => LazySchema(suspend.map(this.apply))
      case RefinementSchema(underlying, refinement) =>
        RefinementSchema(this(underlying), refinement)
      case c @ CollectionSchema(_, _, _, _) => c.copy(member = this(c.member))
      case m @ MapSchema(_, _, _, _) => underlying(m.copy(key = this(m.key), value = this(m.value)))
      case s @ StructSchema(_, _, _, _) => underlying(s.copy(fields = s.fields.map(_.mapK(this))))
    }

}
