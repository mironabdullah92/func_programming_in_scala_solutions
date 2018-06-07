def double(rng: RNG): (Double, RNG) = {
  val (n, newRNG) = nonNegativeInt(rng)
  ((n % Int.MaxValue).toDouble / Int.MaxValue, newRNG)
}