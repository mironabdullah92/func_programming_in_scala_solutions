def composeM[F[_],G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]):
  Monad[({type f[x] = F[G[x]]})#f] = new Monad[({type f[x] = F[G[x]]})#f] {
    def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
    def flatMap[A,B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] = {
      // F.flatMap(fga)(ga => F.map(G.map(T.sequence(ga.map(f)))(join)))
      F.flatMap(fga)(ga => F.map(T.traverse(ga)(f))(G.join))
    }

    /**
     * If F and G would be traversable, we would be able to do it in different way:
        def flatMap[A,B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] = {
          F.flatMap(fga)(ga => L.sequence(flatMap(ga)(a => T.sequence(f(a)))))
        }
     */
  }