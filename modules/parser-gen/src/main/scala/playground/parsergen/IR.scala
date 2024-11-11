package playground.parsergen

import cats.data.NonEmptyList
import treesittersmithy.NodeType
import treesittersmithy.TypeName

enum Type {

  case ADT(name: TypeName, subtypes: NonEmptyList[Subtype])
}

case class Subtype(name: TypeName)

object IR {

  def from(nt: NodeType): Type =
    if nt.subtypes.nonEmpty then fromADT(nt)
    else
      sys.error("todo")

  private def fromADT(nt: NodeType): Type.ADT = Type.ADT(
    name = nt.tpe,
    subtypes = NonEmptyList.fromListUnsafe(nt.subtypes.map(subtype => Subtype(name = subtype.tpe))),
  )

}
