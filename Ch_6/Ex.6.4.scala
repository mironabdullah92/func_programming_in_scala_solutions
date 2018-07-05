def ints1(count: Int)(rng: RNG): (List[Int], RNG) = {
  if (count > 0) {
    val (l,r) = ints(count-1)(rng)
    val (i,r1) = r.nextInt
    (i :: l,r1)
  } else (Nil,rng)
}

// tail-recursive version
def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
  @annotation.tailrec
  def go(c: Int, rngState: RNG, acc: List[Int]): (List[Int], RNG) = {
    if (c > 0) {
      val (i,r) = rngState.nextInt
      go(c-1, r, i :: acc)
    } else (acc, rngState)
  }

  go(count, rng, Nil)
}