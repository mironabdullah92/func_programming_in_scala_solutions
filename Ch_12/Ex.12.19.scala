def compose[G[_]](implicit G: Traverse[G]):
  Traverse[(type f[x] = F[G[x]])#f] = {
    val self = this
    new Traverse[(type f[x] = F[G[x]])#f] {
      override def traverse[H[_],A,B](fa: F[G[A]])(f: A => H[B])(
        implicit H: Applicative[H]): H[F[G[A]]] =
        self.traverse(fa)((ga: G[A] => G.traverse(ga)(f)))
    }
  }