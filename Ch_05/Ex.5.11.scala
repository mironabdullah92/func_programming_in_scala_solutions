def unfold[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  f(z) map(t => Stream.cons(t._1, unfold(t._2)(f))) getOrElse Stream.empty[A]