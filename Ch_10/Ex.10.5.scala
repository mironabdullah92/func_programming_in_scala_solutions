def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
  as.foldLeft(m.zero)((z,a) => m.op(z, f(a)))
