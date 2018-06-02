def sequence[A](os: List[Option[A]]): Option[List[A]] =
  os.foldRight(Some(Nil): Option[List[A]])((o, z) => z.flatMap(l => o.map(a => a :: l)))