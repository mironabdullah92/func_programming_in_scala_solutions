@annotation.tailrec
def runTrampoline[A](ffa: Free[Function0,A]): A = ffa match {
  case Return(a) => a
  case Suspend(r) => r()
  case FlatMap(x, f) => x match {
    case Return(a) => runTrampoline(f(a))
    case Suspend(r) => runTrampoline(f(r()))
    case FlatMap(y, g) => runTrampoline(y flatMap (a => g(a) flatMap f))
  }
}