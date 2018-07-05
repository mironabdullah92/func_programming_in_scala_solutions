def fibs: Stream[Int] = {
  def _fibs(a: Int, b: Int): Stream[Int] =
    Stream.cons(a, _fibs(b, a+b))
  
  _fibs(0, 1)
}