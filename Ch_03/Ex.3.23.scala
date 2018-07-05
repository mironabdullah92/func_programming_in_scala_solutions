// Non-tailrec, check Ex.3.22 for tailrec approach
def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
    case _ => throw new IllegalArgumentException("Lists should be the same size.")
}