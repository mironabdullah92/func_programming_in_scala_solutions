def append[A](l1: List[A], l2: List[A]): List[A] =
  foldRight(l1, l2)((a, l) => Cons(a, l))