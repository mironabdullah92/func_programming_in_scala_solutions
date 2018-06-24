def foldMapV[A,B](v: IndexSeq[A], m: Monoid[B])(f: A => B): B =
  if (v.length <= 0)
    m.zero
  else if (v.length == 1)
    f(v(0))
  else {
    val (l,r) = v.splitAt(v.length / 2)
    m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
  }
