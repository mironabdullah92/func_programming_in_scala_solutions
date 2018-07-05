def functionMonoid[A,B](b: Monoid[B]): Monoid[A => B] =
  new Monoid[A => B] {
    def op(f1: A => B, f2: A => B): A => B =
      a => b.op(f1(a), f2(a))
    
    val zero: A => B = a => b.zero
  }