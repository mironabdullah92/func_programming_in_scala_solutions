def headOption: Option[A] =
  foldRight(None: Option[A])((a, _) => Some(a))