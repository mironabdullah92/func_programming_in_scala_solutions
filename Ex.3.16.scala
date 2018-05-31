def addOneToAll(l: List[Int]): List[Int] =
  foldRight(l, Nil: List[Int])((i, is) => Cons(i + 1, is))
