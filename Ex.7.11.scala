def choiceN[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] =
  es => {
    val n = run(es)(pn).get
    run(es)(choices(n))
  }

def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[B]): Par[A] =
  choiceN(map(cond)(b => if (b) 1 else 0))(List(f,t))