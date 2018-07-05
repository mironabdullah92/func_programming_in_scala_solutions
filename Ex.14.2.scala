def partition[S](arr: STArray[S,Int],
                 n: Int, r: Int, pivot: Int): ST[S,Int] =
for {
  pivotVal <- arr.read(pivot)
  _ <- arr.swap(pivot, r)
  j <- (n until r).foldLeft(ST[S,Int](n))((st, i) => {
    st flatMap (j => arr.read(i)
        flatMap (arri => if (arri < pivotVal)
                            arr.swap(i, j).map(j + 1)
                         else
                            ST[S,Int](j)))
  })
  _ <- swap(j, r)
} yield j

def qs[S](a: STArray[S,Int], n: Int, r: Int): ST[S,Unit] = if (n < r) {
  for {
    pi <- partition(s, n, r, n + (n - r) / 2)
    _ <- qs(n, pi - 1)
    _ <- qs(pi + 1, r)
  } yield ()
} else ST[S,Unit](())