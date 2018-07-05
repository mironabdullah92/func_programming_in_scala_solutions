def takeWhile(p: A => Boolean): Stream[A] =
  foldRight(Stream.empty: Stream[A])((a, z) =>
    if (p(a))
      Stream.cons(a, z)
    else
      Stream.empty)