def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  def op(f1: A => A, f2: A => A) =
    a => f2(f1(a))
  val zero =
    (a: A) => a
}