def doublesToStrings(l: List[Double]): List[String] =
  foldRight(l, Nil: List[String])((a, as) => Cons(a.toString, as))