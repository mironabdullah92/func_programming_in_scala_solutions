def unit[A](a: => A): SGen[A] =
  unsized(Gen.unit(a))

def flatMap[B](f: A => SGen[B]): Gen[B] =
  SGen(i => forSize(i) flatMap {
    f(_).forSize(i)
  })

def map[B](f: A => B): SGen[B] =
  SGen(forSize(_) map f)