def map3[A,B,C,D](fa: F[A],
                  fb: F[B],
                  fc: F[C])(f: (A,B,C) => D): F[D] =
  apply(apply(apply(unit(f.carried))(fa))(fb))(fc)

def map3[A,B,C,D](fa: F[A],
                  fb: F[B],
                  fc: F[C],
                  fd: F[D])(f: (A,B,C,D) => E): F[E] =
  apply(apply(apply(apply(unit(f.carried))(fa))(fb))(fc))(fe)