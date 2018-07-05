def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
  ofa.foldRight(unit(Map[K,V]())) {
    (acc, (k, fv)) => map2(acc, fv)((m, v) => m updated (k, v))
  }