def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
  case Halt() => Halt()
  case Emit(h, t) => Emit(h, this |> t)
  case Await(f2) => this match {
    case Halt() => Halt() |> f2(None)
    case Emit(h, t) => t |> f2(h)
    case Await(f) => Await(i: Option[I] => f(i) |> p2)
  })
}
