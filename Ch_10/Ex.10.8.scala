def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
  def op(p1: Par[A], p2: Par[A]): Par[A] = for {
    a1 <- p1
    a2 <- p2
    m.op(a1, a2)
  } yield m.op(a1, a2)

  val zero: Par[A] = Par.unit(m.zero)
}

def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
  foldMapV(v, par(m))(a => lazyUnit(f(a)))