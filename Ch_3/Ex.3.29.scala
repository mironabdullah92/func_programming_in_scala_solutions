def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
  case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  case Leaf(value) => f(value)
}

def size[A](t: Tree[A]): Int = 
  fold(t)(_ => 1)(1 + _ + _)

def maximum(t: Tree[Int]): Int =
  fold(t)(v => v)(_ max _)

def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
  fold(t)(v => Leaf(f(v)): Tree[B])((bl, br) => Branch(bl,br): Tree[B])

def depth[A](t: Tree[A]): Int =
  fold(t)(_ => 0)(_ max _ + 1)
