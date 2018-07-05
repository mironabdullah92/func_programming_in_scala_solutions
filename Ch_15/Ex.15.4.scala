def count[I]: Process[I,Int] =
  loop(1)((i,n) => (n, n+1))

def mean: Process[Double, Double] = {
  loop((0.0,0))((d, s) => {
    val (sum, n) = s
    val newSum = sum + d
    val newN = n + 1
    (newSum/newN, (newSum, newN))
  })