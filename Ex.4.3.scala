def map2[A,B,C](ao: Option[A], bo: Option[B])(f: (A, B) => C): Option[C] =
  ao.flatMap(a => bo.map(b => f(a, b)))