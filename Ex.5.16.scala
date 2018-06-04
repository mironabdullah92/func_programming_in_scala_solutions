// foldRight version
// Using Cons, so that there is no need to use match. Instead tuple (head, stream) could be used.
def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
  foldRight(Cons(() => z, () => Stream.empty))((a, s) => {
    lazy val head = f(a, s.h())
    lazy val tail = s
    Cons(() => head, () => tail)
  })