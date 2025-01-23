package playground.parsergen

import cats.data.NonEmptyList
import treesittersmithy.FieldName
import treesittersmithy.NodeType
import treesittersmithy.TypeName

enum Type {
  case Union(name: TypeName, subtypes: NonEmptyList[Subtype])
  case Product(name: TypeName, fields: List[Field], children: Option[Children])
}

case class Field(name: FieldName, targetTypes: NonEmptyList[TypeName], repeated: Boolean)
case class Children(targetTypes: NonEmptyList[TypeName], repeated: Boolean)

case class Subtype(name: TypeName)

object IR {

  def from(nt: NodeType): Type =
    if nt.subtypes.nonEmpty then fromUnion(nt)
    else
      fromProduct(nt)

  private def fromUnion(nt: NodeType): Type.Union = Type.Union(
    name = nt.tpe,
    subtypes = NonEmptyList.fromListUnsafe(nt.subtypes.map(subtype => Subtype(name = subtype.tpe))),
  )

  private def fromProduct(nt: NodeType): Type.Product = Type.Product(
    name = nt.tpe,
    fields =
      nt.fields
        .map { (fieldName, fieldInfo) =>
          Field(
            name = fieldName,
            targetTypes = NonEmptyList.fromListUnsafe(fieldInfo.types.map(_.tpe)),
            repeated = fieldInfo.multiple,
          )
        }
        .toList,
    children = nt.children.map { children =>
      Children(
        targetTypes = NonEmptyList.fromListUnsafe(children.types.map(_.tpe)),
        repeated = children.multiple,
      )
    },
  )

}
