def isOrdered(is: IndexSeq[Int]): Boolean =
  foldMap(is.toList, new Monoid[Option[Option[(Int, Int)]]] {
    def op(x: Option[Option[(Int, Int)]], y: Option[Option[(Int, Int)]]) = (x, y) match {
      case (Some(None), _) => y
      case (_, Some(None)) => x
      case (Some(Some(xMin, xMax)), Some(Some(yMin, yMax))) if xMax <= yMin =>
        Some(Some(xMin, yMax))
      case _ => None
    }
  })(i => (i, i)) match {
    case None => false
    case _ => true
  }