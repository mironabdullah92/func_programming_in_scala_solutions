def filter[A](as: List[A])(f: A => Boolean): List[A] =
  flatMap(as)((a) => if (f(a)) List(a) else Nil)