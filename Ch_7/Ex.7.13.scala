def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
  run(es)(choices(run(es)(pa).get))