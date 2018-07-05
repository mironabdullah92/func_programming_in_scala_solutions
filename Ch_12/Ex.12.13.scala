val listTraverse = new Traverse[List] {
  override def traverse[G[_],A,B](as: List[A])(f: A => G[B]): G[List[B]] =
    as.foldRight(G.unit(List[B]()))((acc, a) => map2(acc, f(a))(_ :: _)
}

val optionTraverse = new Traverse[Option] {
  override def traverse[G[_],A,B](oa: Option[A])(f: A => G[B]): G[Option[B]] = oa match {
    Some(a) => map(f(a))(Some(_))
    None    => G.unit(None)
  }
}

case class Tree[A](head: A, tail: List[Tree[A]])

val treeTraverse = new Traverse[Tree] {
  override def traverse[G[_],A,B](ta: Tree[A])(f: A => G[B]): G[Tree[A]] =
    G.map2(f(h), listTreverse.traverse(t)(tree => traverse(tree)(f)))(Tree(_, _))
}