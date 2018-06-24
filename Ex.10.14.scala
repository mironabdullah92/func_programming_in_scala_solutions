object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as.map(f) getOrElse mb.zero
  
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as.map(f(z, _)) getOrElse z
  
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as.map(f(a, z)) getOrElse z
}