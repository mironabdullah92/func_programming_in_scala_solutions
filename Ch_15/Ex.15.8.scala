def exists[I](f: I => Boolean): Process[I,Boolean] =
  map(f) |> loop(false)((i: Boolean, s) => (i || s, i || s))
