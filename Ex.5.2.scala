// Part of the Stream[+A] trait.

def take(n: Int): Stream[A] =
  if (n > 0)
    this match {
      case Cons(h, t) => Stream.cons(h(), t().take(n-1))
      case _ => Stream.empty
    }
  else
    Stream.empty

def drop(n: Int): Stream[A] = this match {
  case Cons(_, t) if n > 0 => t().drop(n-1)
  case _ => this
}