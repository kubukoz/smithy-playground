package playground.smithyql

import cats.Id

// Some pretty-printing utils for diagnostics
object PrettyPrint {

  def printList[A](
    l: List[A]
  )(
    ppContent: A => String
  ): String = l.map(ppContent).mkString("List(", ", ", ")")

  def escapeString(
    s: String
  ) = Formatter.renderStringLiteral(s).replace("\\", "\\\\").replace("\"", "\\\"")

//
  def prettyPrintWithComments[A](withSource: WithSource[A])(ppContent: A => String): String =
    s"""WithSource(
      commentsLeft = ${printList(withSource.commentsLeft)(c => s"Comment(${escapeString(c.text)})")},
      commentsRight = ${printList(withSource.commentsRight)(c =>
      s"Comment(${escapeString(c.text)})"
    )},
      value = ${ppContent(withSource.value)},
    )"""

//
  def prettyPrint(q: Query[WithSource]): String = {
    def prettyPrintNode(node: InputNode[WithSource]): String =
      node match {
        case StringLiteral(ss) => s"StringLiteral(${prettyPrintWithComments(ss)(escapeString)})"
        case IntLiteral(ss)    => s"IntLiteral(${prettyPrintWithComments(ss)(_.toString)})"
        case s @ Struct(_)     => prettyPrintStruct(s)
      }

    def prettyPrintStruct(s: Struct[WithSource]): String =
      s"""Struct[WithSource](
        ${prettyPrintWithComments(s.fields)(
        prettyPrintWithComments(_)(
          _.map { case (k, v) =>
            s"${prettyPrintWithComments(k)(kk => s"Key(${escapeString(kk.text)})")} -> ${prettyPrintNode(v)},\n"
          }
            .mkString("Map(", "\n", ")")
        )
      )}
      )"""

    s"""Query[WithSource](operationName = ${prettyPrintWithComments(q.operationName)(n =>
      s"OperationName(${escapeString(n.text)})"
    )}, input = ${prettyPrintStruct(q.input)})"""
  }

  case class Structure(keys: Map[String, Structure]) {

    def render(depth: Int): String = {
      val indent = "  " * depth
      keys
        .map { case (k, v) => s"$indent$k:\n${v.render(depth + 1)}" }
        .mkString("\n")
    }

  }

  def empty = Structure(Map.empty)

  def just(k: String) = Structure(Map(k -> empty))

  def toStructure: InputNode[Id] => Structure = _.fold(
    struct =
      fields =>
        Structure(
          fields
            .fields
            .map { case (k, v) => k.text -> toStructure(v) }
            .toMap
        ),
    string = s => Structure(Map("string" -> just(s.value))),
    int = i => Structure(Map("int" -> just(i.value.toString))),
  )

}
