def map[B](f: A => B): Stream[B] =
  unfold(this)((s) => s match {
    case Cons(h,t) => Some((f(h()), t()))
    case _ => None
  })

def take(n: Int): Stream[A] =
  unfold((this, n)) {
    case (Cons(h,t), i) if i > 0 => Some((h(), (t(), i-1)))
    case _ => None
  }

def takeWhile(p: A => Boolean): Stream[A] =
  unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

def zipWith[B,C](s: Stream[B])(f: (A,B) => C): Stream[C] =
  unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

def zipAll[B](s: Stream[B]): Stream[(Option[A],Option[B])] =
  unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), _) => Some(((Some(h1()), None), (t1(), Stream.empty)))
    case (_, Cons(h2, t2)) => Some(((None, Some(h2())), (Stream.empty, t2())))
    case _ => None
  }