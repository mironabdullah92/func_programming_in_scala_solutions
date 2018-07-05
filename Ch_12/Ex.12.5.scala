def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
  new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: => A): Either[E, A] = Some(a)
    def flatMap[A,B](eea: Either[E, A])(f: A => Either[E, B]) = eea match {
      Left(e) => Left(e)
      Right(a) => f(a)
    }
  }