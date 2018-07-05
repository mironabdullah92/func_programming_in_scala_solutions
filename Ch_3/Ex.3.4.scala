def drop[A](l: List[A], n: Int): List[A] = 
  if (n == 0) l
  else l match {
    case Nil => throw new IllegalArgumentException("Not enough elements to drop.")
    case Cons(_, t) => drop(t, n-1)
  }