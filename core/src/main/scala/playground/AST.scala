package playground

case class Query(
  operationName: String,
  input: Struct,
)

//todo bad name, Query isn't an AST but is part of the AST
sealed trait AST extends Product with Serializable

case class Struct(
  fields: Map[String, AST]
) extends AST

case class IntLiteral(value: Int) extends AST
case class StringLiteral(value: String) extends AST
