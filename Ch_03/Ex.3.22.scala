def addTwoLists(l1: List[Int], l2: List[Int]): List[Int] = {
  @annotation.tailrec
  def go(as1: List[Int], as2: List[Int], acc: List[Int]): List[Int] = (as1, as2) match {
    case (Nil, Nil) => acc
    case (Cons(h1, t1), Cons(h2, t2)) => go(t1, t2, Cons(h1+h2, acc))
    case _ => throw new IllegalArgumentException("Lists should be the same size.")
  }

  reverse(go(l1, l2, Nil: List[Int]))
}