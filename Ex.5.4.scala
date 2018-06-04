  def forAll(p: A => Boolean): Boolean =
    foldRight(true)(p(_) && _ )