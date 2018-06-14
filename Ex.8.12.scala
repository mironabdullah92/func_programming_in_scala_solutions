def listOf[A](g: Gen[A]): SGen[List[A]] =
  SGen(i => listOfN(i, g))