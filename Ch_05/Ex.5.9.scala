def from(i: Int): Stream[Int] =
  Stream.cons(i, from(i+1))