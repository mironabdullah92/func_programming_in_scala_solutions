def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
  rng => {
    val (a,rng2) = ra(rng)
    val (b,rng3) = rb(rng2)
    (f(a,b), rng3)
  }