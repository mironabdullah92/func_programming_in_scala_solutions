def optionMonoid[A] = Monoid[Option[A]] {
  def op(o1: Option[A], o2: Option[A]) =
    o1 orElse o2
  val zero = None
}