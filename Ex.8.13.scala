def listOf1[A](g: Gen[A]): SGen[List[A]] =
  SGen(i => listOfN(i max 1, g))

// Testing code won't change, besides the fact
// that we will use listOf1 instead of listOf.
