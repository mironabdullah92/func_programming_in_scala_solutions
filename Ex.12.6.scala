def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =
  new Applicative[({type f[x] = Validation[E,x]})#f] {
    def unit[A](a: => A): Validation[E,A] = Success(a)
    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = (fab, fa) match {
      (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
      (e@Failure(_,_), _) => e
      (_, e@Failure(_,_)) => e
      (Success(ab), Success(a)) => Success(ab(a))
    }
  }