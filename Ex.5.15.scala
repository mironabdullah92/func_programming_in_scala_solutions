def tails: Stream[Stream[A]] =
  unfold(Some(this): Option[Stream[A]])(s => s match {
    case Some(s) => s match {
      case Cons(h,t) => Some((s, Some(t())))
      case _ => Some((Stream.empty, None))
    }
    case _ => None
  })