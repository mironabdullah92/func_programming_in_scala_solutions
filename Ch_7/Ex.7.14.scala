def join[A](a: Par[Par[A]]): Par[A] =
  es => run(es)(run(es)(a).get)

def flatMapViaJoin[A,B](a: Par[A])(f: A => Par[B]): Par[B] =
  join(map(a)(f))

def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
  flatMap(a)(p => p)