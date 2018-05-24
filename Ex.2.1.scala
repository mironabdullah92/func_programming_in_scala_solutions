def fib(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, num1: Int, num2: Int): Int =
    if (n == 1)
      num1
    else
      go(n - 1, num2, num1 + num2)
  
  go(n, 0, 1)
}