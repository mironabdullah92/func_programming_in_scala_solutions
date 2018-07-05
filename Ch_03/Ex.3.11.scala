def sum(ints: List[Int]): Int =
  foldLeft(ints, 0)(_ + _)

def product(ds: List[Double]): Double =
  foldLeft(ds, 1.0)(_ * _)

def length[A](l: List[A]): Int =
  foldLeft(l, 0)((acc, _) => acc + 1)