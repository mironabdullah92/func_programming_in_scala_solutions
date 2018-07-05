def maximum(t: Tree[Int]): Int = t match {
  case Branch(l, r) => maximum(l) max maximum(r)
  case Leaf(value) => value
}