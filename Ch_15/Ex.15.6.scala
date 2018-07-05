def zipWithIndex: Process[I, (I,Int)] =
  loop(0)((i, n) => ((i, n), (n+1)))