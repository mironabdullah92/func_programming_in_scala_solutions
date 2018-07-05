sealed trait Stream[+A] {
  // 5.1

  // Recursively
  // def toList: List[A] = this match {
  //   case Empty => Nil
  //   case Cons(h, t) => h() :: t().toList
  // }
  
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }

    go(this, Nil).reverse
  }

  // 5.2
  def take(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h,t), i) if i > 0 => Some((h(), (t(), i-1)))
      case _ => None
    }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B,C](s: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  
  def zipAll[B](s: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), _) => Some(((Some(h1()), None), (t1(), Stream.empty)))
      case (_, Cons(h2, t2)) => Some(((None, Some(h2())), (Stream.empty, t2())))
      case _ => None
    }
  
  def startsWith[A](s: Stream[A]): Boolean = 
    zipAll(s).takeWhile {
      case (_, Some(_)) => true
      case _ => false
    } forAll {
      case (h,h2) => h == h2
    }
  
  def tails: Stream[Stream[A]] =
    unfold(Some(this): Option[Stream[A]])(s => s match {
      case Some(s) => s match {
        case Cons(h,t) => Some((s, Some(t())))
        case _ => Some((Stream.empty, None))
      }
      case _ => None
    })

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)(p(_) && _ )

  // 5.6
  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))
  
  // 5.7

  def unfold[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) map(t => Stream.cons(t._1, unfold(t._2)(f))) getOrElse Stream.empty[A]
    
  def map[B](f: A => B): Stream[B] =
    unfold(this)((s) => s match {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    })

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h,t) =>
      if (p(h))
        Stream.cons(h, t)
      else
        t
    )

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((h, t) => Stream.cons(h, t))
  
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => f(h) append t)

  // foldRight version
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(Cons(() => z, () => Stream.empty))((a, s) => {
      lazy val head = f(a, s.h())
      lazy val tail = s
      Cons(() => head, () => tail)
    })

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}