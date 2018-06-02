// I could use map2, but then the function will always traverse entire list
// without short-cut evaluation when one of options is None.
def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case h :: t => f(h).flatMap((hh) => traverse(t)(f).map(hh :: _))
  case Nil => Some(Nil)
}

def sequence[A](os: List[Option[A]]): Option[List[A]] =
  traverse(os)(o => o)