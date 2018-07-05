def tail[A](l: List[A]): List[A] = l match {
  case Nil => throw new IllegalArgumentException("Nil doesn't have a tail.")
  case Cons(_, t) => t
}