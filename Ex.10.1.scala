val intAddition = Monoid[Int] {
  def op(i1: Int, i2: Int) = i1 + i2
  val zero = 0
}

val intMultiplication = Monoid[Int] {
  def op(i1: Int, i2: Int) = i1 * i2
  val zero = 1
}

val booleanOr = Monoid[Boolean] {
  def op(b1: Boolean, b2: Boolean) = b1 || b2
  val zero = false
}

val booleanAnd = Monoid[Boolean] {
  def op(b1: Boolean, b2: Boolean) = b1 && b2
  val zero = true
}