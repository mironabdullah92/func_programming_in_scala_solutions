def join[A](mma: F[F[A]]): F[A] =
  mma flatMap(ma => ma)