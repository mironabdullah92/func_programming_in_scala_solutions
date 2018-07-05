def product[G[_]](G: Applicative[G]):
  Applicative[({type f[x] = (F[x], G[x])})#f] = {
  val self = this
  new Applicative[({type f[x] = (F[x], G[x])})#f] {
    def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
    override apply[A,B](ft: (F[A => B], G[A => B]))(p: (F[A], G[A])): (F[B], G[B]) =
      (self.apply(ft._1)(p._1), G.apply(ft._2)(p._2))
  }
}