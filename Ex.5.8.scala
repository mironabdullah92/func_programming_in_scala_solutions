def constant[A](a: A): Stream[A] =
  Stream.cons(a, constant(a))