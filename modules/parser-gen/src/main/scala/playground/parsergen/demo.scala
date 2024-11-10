case class Foo(bars: List[Bar]) {
  def select[A](f: Foo.Selector => Selection[A]): List[A] = f(Foo.Selector(List(this))).path
}

object Foo {

  case class Selector(path: List[Foo]) extends Selection[Foo] {
    def bars: Bar.Selector = Bar.Selector(path.flatMap(_.bars))
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

trait Selection[A] {
  def path: List[A]
}

@main def demo = {
  import util.chaining.*

  Foo(List(Bar(Some(Baz(List("a", "b").map(Identifier.apply)))))).select(_.bars.baz).pipe(println)
  Foo(List(Bar(Some(Baz(List("a", "b").map(Identifier.apply))))))
    .select(_.bars.baz.names)
    .pipe(println)
}
