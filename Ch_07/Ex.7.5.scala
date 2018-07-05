def sequence[A](ps: List[Par[A]]): Par[List[A]] =
  ps.foldRight(unit[List[A]](Nil))((pa,pl) => map2(pa,pl)(_ :: _))