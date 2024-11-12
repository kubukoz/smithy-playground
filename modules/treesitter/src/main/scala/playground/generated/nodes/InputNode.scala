// Generated code! Do not modify by hand.
package playground.generated.nodes

import org.polyvariant.treesitter4s.Node
import playground.treesitter4s.std.Selection
import annotation.nowarn

opaque type InputNode <: Node = Boolean_ | List_ | Null_ | Number | String_ | Struct

object InputNode {

  extension (node: InputNode) {
    def asBoolean: Option[Boolean_] = Boolean_.unapply(node)
    def asList: Option[List_] = List_.unapply(node)
    def asNull: Option[Null_] = Null_.unapply(node)
    def asNumber: Option[Number] = Number.unapply(node)
    def asString: Option[String_] = String_.unapply(node)
    def asStruct: Option[Struct] = Struct.unapply(node)
    def visit[A](visitor: Visitor[A]): A = visitor.visit(node)
  }

  def apply(node: Node): Either[String, InputNode] = node match {
    case Boolean_(node) => Right(node)
    case List_(node) => Right(node)
    case Null_(node) => Right(node)
    case Number(node) => Right(node)
    case String_(node) => Right(node)
    case Struct(node) => Right(node)
    case _ => Left(s"Expected InputNode, got ${node.tpe}")
  }

  def apply(node: Boolean_ | List_ | Null_ | Number | String_ | Struct): InputNode = node

  def unsafeApply(node: Node): InputNode = apply(node).fold(sys.error, identity)

  def unapply(node: Node): Option[InputNode] = apply(node).toOption


  trait Visitor[A] {
    def onBoolean(node: Boolean_): A
    def onList(node: List_): A
    def onNull(node: Null_): A
    def onNumber(node: Number): A
    def onString(node: String_): A
    def onStruct(node: Struct): A

    def visit(node: InputNode): A = (node: @nowarn("msg=match may not be exhaustive")) match {
      case Boolean_(node) => onBoolean(node)
      case List_(node) => onList(node)
      case Null_(node) => onNull(node)
      case Number(node) => onNumber(node)
      case String_(node) => onString(node)
      case Struct(node) => onStruct(node)
    }
  }

  object Visitor {
    abstract class Default[A] extends Visitor[A] {
      def default: A

      def onBoolean(node: Boolean_): A = default
      def onList(node: List_): A = default
      def onNull(node: Null_): A = default
      def onNumber(node: Number): A = default
      def onString(node: String_): A = default
      def onStruct(node: Struct): A = default
    }
  }

  final case class Selector(path: List[InputNode]) extends Selection[InputNode] {
    def boolean : Boolean_.Selector = Boolean_.Selector(path.flatMap(_.asBoolean))
    def list : List_.Selector = List_.Selector(path.flatMap(_.asList))
    def `null` : Null_.Selector = Null_.Selector(path.flatMap(_.asNull))
    def number : Number.Selector = Number.Selector(path.flatMap(_.asNumber))
    def string : String_.Selector = String_.Selector(path.flatMap(_.asString))
    def struct : Struct.Selector = Struct.Selector(path.flatMap(_.asStruct))

    type Self = Selector
    protected val remake = Selector.apply
  }
}
