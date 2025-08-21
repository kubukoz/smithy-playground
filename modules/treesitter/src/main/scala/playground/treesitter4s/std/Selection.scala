package playground.treesitter4s.std

trait Selection[A] {
  type Self <: Selection[A]
  def path: List[A]
  protected def remake: List[A] => Self

  def transform(f: List[A] => List[A]): Self = remake(f(path))
  def find(f: A => Boolean): Self = transform(_.find(f).toList)
}
