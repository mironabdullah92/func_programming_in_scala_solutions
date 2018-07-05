case class Gen[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfNDynamic(gn: Gen[Int]): Gen[List[A]] =
    gn.flatMap(n => listOfN(n, this))
}