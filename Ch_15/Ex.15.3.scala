def mean: Process[Double, Double] = {
  def go(sum: Double, n: Double): Process[Double, Double] =
    Await {
      case Some(d) => Emit((sum+d)/(n+1), go(sum+d, n+1))
      case None => Halt()
    }
  
  go(0.0, 0.0)
}