@annotation.tailrec
def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
  case Nil => false
  case h :: t =>
    if ((sup.take(sub.length), sub).zipped.forall((a: (A,A)) => a match { case (l, r) => l == r }))
      true
    else
      hasSubsequence(t, sub)
}