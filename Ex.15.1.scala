def await[I](f: I => Process[I,I]): Process[I,I] = Await {
  case Some(i) => f(i)
  case None => Halt()
}

def take[I](n: Int): Process[I,I] =
  if (n > 0) await(i => Emit(i, take[I](n-1)))
  else Halt()

def drop[I](n: Int): Process[I,I] =
  if (n > 0) await(_ => Await(drop[I](n-1))
  else lift(i => i)

def takeWhile[I](p: I => Boolean): Process[I,I] =
  Await {
    case Some(i) if (p(i)) => Emit(i, takeWhile(p)))
    case _ => Halt()
  }

def dropWhile[I](p: I => Boolean): Process[I,I] =
  await { i => 
    if (p(i)) Await(dropWhile(p))
    else Emit(i, lift(a => a).repeat)
  }