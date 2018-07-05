def unit[A](a: A): Rand[A] =
  rng => (a,rng)

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  fs.foldRight[Rand[List[A]]](unit(Nil))((a,as) => map2(a,as)(_ :: _))

val int: Rand[Int] = _.nextInt
def ints(n: Int): Rand[List[Int]] =
  sequence(List.fill(n)(int))