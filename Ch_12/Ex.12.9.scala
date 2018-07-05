def compose[G[_]](G: Applicative[G]):
  Applicative[({type f[x] = F[G[x]]})#f] = {
  val self = this
  new Applicative[({type f[x] = F[G[x]]})#f] {
    def unit[A](a: => A): (F[A], G[A]) = self.unit(G.unit(a))
    override apply[A,B](fgab: F[G[A => B]])(fp: F[G[A]]): F[G[B]] =
      self.map2(fgab, fp)((gab, p) => G.apply(gab)(p))
  }
}