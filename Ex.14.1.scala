def fill(xs: Map[Int,A]): ST[S,Unit] = {
  xs.foldLeft(ST[S,Unit](())) ((st, kv) => {
    val (k,v) = kv
    st flatMap (_ => write(k, v))
  })
}