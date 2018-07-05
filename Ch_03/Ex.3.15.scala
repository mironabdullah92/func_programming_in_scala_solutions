def flatten[A](l: List[List[A]]): List[A] =
  foldRight(l, Nil: List[A])(foldRight(_, _)(Cons(_, _)))