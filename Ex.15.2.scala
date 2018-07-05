def count[I]: Process[I,Int] = {
  def go(n: Int): Process[I,Int] =
    Await {
      case Some(i) => Emit(n, go(n+1))
      case None => Halt()
    }
  
  go(1)
}