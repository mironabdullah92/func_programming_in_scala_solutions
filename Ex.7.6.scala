def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
  as.map(asyncF(a => {
    if (f(a))
      Some(a)
    else
      None
  })).foldRight(unit[List[A]](Nil))((pa,pl) => {
    map2(pa,pl)((o, l) => o match {
        case None => l
        case Some(a) => a :: l
      })
  })