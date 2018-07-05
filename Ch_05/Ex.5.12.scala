def fibs(): Stream[Int] =
  unfold((0, 1))((t) => Some((t._1, (t._2, t._1 + t._2))))

def from(i: Int): Stream[Int] =
  unfold(i)(s => Some((s, s + 1)))

def constant[A](a: A): Stream[A] =
  unfold(a)(s => Some((s, s)))

def ones(): Stream[Int] =
  unfold(1)((_) => Some(1, 1))