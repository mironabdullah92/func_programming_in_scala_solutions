def filter[A](as: List[A])(f: A => Boolean): List[A] =
  foldRight(as, Nil: List[A])((elem, list) => if (f(elem)) Cons(elem, list) else list)