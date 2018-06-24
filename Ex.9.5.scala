// User nonStrictParser that converts strict p to non-strict
def nonStrictParser[A](p: => Parser[A]): Parser[A]