@annotation.tailrec
def step[F[_],A](ffa: Free[F,A])(F: Monad[F]): Free[F,A] = ffa match {
  case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g)
  case FlatMap(Return(x), f) => step(f(x))
  case _ => ffa
}

def run[F[_],A](ffa: Free[F,A])(implicit F: Monad[F]): F[A] = step(ffa) match {
  case Return(a) => F.unit(a)
  case Suspend(r) => r
  case FlatMap(x, f) => x match {
    case Suspend(r) => F.flatMap(r)(a => run(f(a)))
    case _ => sys.error("Impossible; `step` eliminates these cases")
  }
}
