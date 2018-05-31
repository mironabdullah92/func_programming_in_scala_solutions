def reverse[A](l: List[A]): List[A] =
  foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))