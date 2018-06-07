// Book has incorrect code. It provides the following example:
// def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
//   val (i, rng2) = nonNegativeInt(rng)
//   val mod = i % n
//   if (i + (n-1) - mod >= 0)
//     (mod,rng2)
//   else nonNegativeLessThan(n)(rng)
// }
//
// In this code we retry with same rng:
//   else nonNegativeLessThan(n)(rng)
// This will lead to infinity loop.
// Correct code should be:
//   else nonNegativeLessThan(n)(rng2)

def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
  val (a,r) = f(rng)
  g(a)(r)
}

def nonNegativeLessThan(n: Int): Rand[Int] =
  flatMap(nonNegativeInt)(i => {
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      unit(mod)
    else
      nonNegativeLessThan(n)
  })