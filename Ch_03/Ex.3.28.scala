def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
  case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  case Leaf(value) => Leaf(f(value))
}