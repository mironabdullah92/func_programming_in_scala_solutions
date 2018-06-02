// Same as Ex.4.5.scala
// I could use map2, but then the function will always traverse entire list
// without short-cut evaluation when one of options is Left.
def traverse[E, A, B](as: List[A])(f: A => Either[E, B]):
  Either[E, List[B]] = as match {
    case h :: t => f(h).flatMap((hh) => traverse(t)(f).map(hh :: _))
    case Nil => Right(Nil)
  }

def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
  traverse(es)(e => e)