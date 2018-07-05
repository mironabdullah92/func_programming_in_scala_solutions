def flMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  def op(f1: A => A, f2: A => A) =
    b => f2.compose(f1)(b)
  val zero = (a: A) => a
}

def frMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  def op(f1: A => A, f2: A => A) =
    b => f1.compose(f2)(b)
  val zero = (a: A) => a
}

def foldLeft[A,B](as: List[A])(z: B)(f: (B, A) => B): B =
  foldMap(as, flMonoid[B])(a => b => f(b,a))(z)

def foldRight[A,B](as: List[A])(z: B)(f: (A,B) => B): B =
  foldMap(as, frMonoid[B])(a => b => f(a,b))(z)