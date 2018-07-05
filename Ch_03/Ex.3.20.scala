def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
  foldRight(map(as)(f), Nil: List[B])(
    (l, acc) => foldRight(l, acc)(Cons(_, _)))