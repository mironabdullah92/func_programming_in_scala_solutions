def map[A,B](fa: F[A])(f: A => B): F[B] =
  traverse(fa)(Some(f(_))) match { case Some(fb) => fb}