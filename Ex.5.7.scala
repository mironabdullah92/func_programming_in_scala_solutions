// Lazy, but first value will be computed in non-lazy way
def map[B](f: A => B): Stream[B] = 
  foldRight(Stream.empty[B])((h,t) => Stream.cons(f(h), t))

// Fully lazy solution
def map1[B](f: A => B): Stream[B] = this match {
  case Cons(h, t) => Stream.cons(f(h()), t().map(f))
  case _ => Stream.empty[B]
}

def filter(p: A => Boolean): Stream[A] =
  foldRight(Stream.empty[A])((h,t) =>
    if (p(h))
      Stream.cons(h, t)
    else
      t
  )

def append[B >: A](s: Stream[B]): Stream[B] =
  foldRight(s)((h, t) => Stream.cons(h, t))

def flatMap[B](f: A => Stream[B]): Stream[B] =
  foldRight(Stream.empty[B])((h, t) => f(h) append t)