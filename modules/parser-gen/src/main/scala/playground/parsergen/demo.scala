case class Foo(bars: List[Bar], nodes: List[Node]) {
  def select[A](f: Foo.Selector => Selection[A]): List[A] = f(Foo.Selector(List(this))).path
}

object Foo {

  case class Selector(path: List[Foo]) extends Selection[Foo] {
    def bars: Bar.Selector = Bar.Selector(path.flatMap(_.bars))
    def nodes: Node.Selector = Node.Selector(path.flatMap(_.nodes))
  }

}

case class Bar(baz: Option[Baz])

object Bar {

  case class Selector(path: List[Bar]) extends Selection[Bar] {
    def baz: Baz.Selector = Baz.Selector(path.flatMap(_.baz))

  }

}

case class Baz(names: List[Identifier])

object Baz {

  case class Selector(path: List[Baz]) extends Selection[Baz] {
    def names: Identifier.Selector = Identifier.Selector(path.flatMap(_.names))
  }

}

case class Identifier(value: String)

object Identifier {

  case class Selector(path: List[Identifier]) extends Selection[Identifier]
}

enum Node {
  case Ident(i: Identifier)
  case B(b: Baz)

  def asIdent: Option[Identifier] =
    this match {
      case Ident(i) => Some(i)
      case _        => None
    }

  def asB: Option[Baz] =
    this match {
      case B(b) => Some(b)
      case _    => None
    }

}

object Node {

  case class Selector(val path: List[Node]) extends Selection[Node] {
    def ident: Identifier.Selector = Identifier.Selector(path.flatMap(_.asIdent))
    def b: Baz.Selector = Baz.Selector(path.flatMap(_.asB))
  }

}

trait Selection[A] {
  def path: List[A]
}

@main def demo = {
  import util.chaining.*

  Foo(
    List(Bar(Some(Baz(List("a", "b").map(Identifier.apply))))),
    List(Node.Ident(Identifier("aa"))),
  ).select(_.bars.baz).pipe(println)

  println(
    Foo(
      List(Bar(Some(Baz(List("a", "b").map(Identifier.apply))))),
      List(Node.Ident(Identifier("aa"))),
    )
      .select(_.bars.baz.names) ==
      Foo(
        List(Bar(Some(Baz(List("a", "b").map(Identifier.apply))))),
        List(Node.Ident(Identifier("aa"))),
      )
        .select(_.bars.baz)
        .flatMap(_.names)
  )

  Foo(
    List(Bar(Some(Baz(List("a", "b").map(Identifier.apply))))),
    List(Node.Ident(Identifier("aa"))),
  )
    .select(_.nodes.ident)
    // .select(_.nodes.b.names)
    .pipe(println)
}
