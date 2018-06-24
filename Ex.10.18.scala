val m: Monoid[Map[A, Int]] = new Monoid[Map[A, Int]] {
  def op(left: Map[A, Int], right: Map[A, Int]): Map[A, Int] =
    foldLeft(left.keySet ++ right.keySet)(zero) { (z, k) =>
        z updated (k -> right.getOrElse(k, 0) + left.getOrElse(k, 0))
      }
  
  val zero: Map[A, Int] = Map()
}

def bag[A](as: IndexSeq[A]): Map[A, Int] =
  foldMap(as)(Map(_ -> 1))(m)