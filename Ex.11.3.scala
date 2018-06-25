/*
def sequence[A](lma: List[F[A]]): F[List[A]] =
  lma.foldRight(unit[List[A]](Nil))((ma, ml) => for {
    a <- ma
    l <- ml
  } yield (a :: l))
*/

def sequence[A](lma: List[F[A]]): F[List[A]] =
  lma.foldRight(unit(List[A]()))((ma, ml) => map2(ma, ml)(_ :: _))

def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
  la.foldRight(unit(List[A]()))((a, ml) => map2(f(a), ml)(_ :: _))