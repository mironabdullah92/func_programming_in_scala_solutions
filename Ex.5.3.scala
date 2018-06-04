def takeWhile(p: A => Boolean): Stream[A] = this match {
  case Cons(h, t) if (p(h())) => Stream.cons(h(), t().takeWhile(p))
  case _ => Stream.empty
}