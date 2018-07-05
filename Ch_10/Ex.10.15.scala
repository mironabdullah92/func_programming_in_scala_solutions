def toList[A](fa: F[A]): List[A] =
  foldRight(fa)(Nil: List[A])(_ :: _)