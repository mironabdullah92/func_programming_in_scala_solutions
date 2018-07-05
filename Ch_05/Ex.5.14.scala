def startsWith[A](s: Stream[A]): Boolean = 
  zipAll(s).takeWhile {
    case (_, Some(_)) => true
    case _ => false
  } forAll {
    case (h,h2) => h == h2
  }