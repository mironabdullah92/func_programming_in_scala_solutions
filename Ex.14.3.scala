sealed abstract class STMap[S,K,V](implicit manifest: Manifest[A]) {
  protected def value: Map[K,V]
  def read(k: K): ST[S,V] =
    ST(value(k))
  
  def add(k: K, v: V): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      value(k) = v
      ((), s)
    }
  }

  def remove(k: K): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      value -= k
      ((), s)
    }
  }

  def keys: ST(value.keys)
}

object STMap {
  def empty[S,K,V]: ST[S, STMap[S,K,V]] = ST(new STMap[S,K,V] {
    val value = HashMap.empty[K,V]
  })
}