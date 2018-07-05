def foldLeft[A,B](fa: F[A])(z: B)(f: (B,A) => B): B =
  mapAccum(fa, z)((a, s) => ((), f(s, a))._2