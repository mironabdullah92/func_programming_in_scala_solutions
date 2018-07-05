val wcMonoid: Monoid[WC] = new Monoid[WC] {
  def op(wc1: WC, wc2: WC): WC = (wc1, wc2) match {
    case (Stub(s1), Stub(s2)) => Stub(s1+s2)
    case (Stub(s1), Part(l, n, r) => Part(s1+l, n, r))
    case (Part(l, n, r), Stub(s2)) => Part(l, n, r+s2)
    case (Part(l1, n1, ""), Part("", n2, r2)) => Part(l1, n1+n2, r2)
    case (Part(l1, n1, r2), Part(l2, n2, r2)) => Part(l1, n1+n2+1, r2)
  }

  val zero = Stub("")
}