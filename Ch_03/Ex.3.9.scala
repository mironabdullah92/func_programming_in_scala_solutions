def length[A](as: List[A]): Int =
  foldRight(as, 0)((_, acc) => acc + 1)
