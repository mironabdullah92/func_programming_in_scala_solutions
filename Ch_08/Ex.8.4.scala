case class Gen[A](sample: State[RNG,A])

def choose(start: Int, stopExclusive: Int): Gen[Int] =
  Gen(State(nonNegativeInt).map(n => n % (stopExclusive - start) + start))