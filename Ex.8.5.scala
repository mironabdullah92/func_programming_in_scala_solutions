def unit[A](a: => A): Gen[A] =
  Gen(State.unit(a))

def boolean: Gen[Boolean] =
  Gen(State(nonNegativeInt).map(_ % 2 == 0))

def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
  Gen(State.sequence(List.fill(n)(g.sample)))