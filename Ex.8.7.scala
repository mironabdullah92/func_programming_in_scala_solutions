// See Ex.8.8.scala
// def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
//   weighted((g1, 0.5), (g2, 0.5))

def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
  boolean.flatMap(bool => if (bool) g1 else g2)