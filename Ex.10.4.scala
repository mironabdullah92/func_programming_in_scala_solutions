def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
  forAll(for {
    a <- gen
    b <- gen
    c <- gen
  } yield (a, b, c))(t => {
    val (a, b, c) = t
    m.op(m.op(a, b), c) == m.op(a, m.op(b, c))
  }) &&
  forAll(gen)(a => m.op(a, m.zero) == a)