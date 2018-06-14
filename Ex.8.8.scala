def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
  Gen(State(nonNegativeInt)).flatMap(i =>
    if (i / Int.MaxValue < g1._2)
      g1._1
    else
      g2._1
    ))