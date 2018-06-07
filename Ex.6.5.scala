type Rand[+A] = RNG => (A, RNG)

def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

val double: Rand[Double] = 
  map(nonNegativeInt)(i => (i % Int.MaxValue).toDouble / Int.MaxValue)