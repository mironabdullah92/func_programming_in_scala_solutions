def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
  ms.foldRight(unit(List[A]()))((a, mla) => map2(f(a), mla){ (p, l) =>
    if (p)  a :: l
    else    l
    })