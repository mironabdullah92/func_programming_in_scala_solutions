type Parser[+A] = Location => Result[A]

trait Result[+A]
case class Success[+A](get: A, charConsumed: Int) extends Result[A]
case class Failure(get: ParseError) extends Result[Nothing]

case class Location(input: String, offset: Int = 0 {
  lazy val line = input.slice(0, offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset+1).lasIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }
})

def string(s: String): Parser[String] =
  loc: Location => {
    val inputStr = loc.input.slice(offset, s.length)
    if (inputStr == s)
      Success(s, s.length)
    else
      Failure(loc.toError(s"Expected $s, but found %inputStr"))
  }

def succeed[A](s: A): Parser[A] =
  loc: Location => Success(s, s.length)

def regex(r: Regex): Parser[String] =
  loc: Location => r.findPrefixOf(loc.input) match {
    None => Failure(s"Failed regex ${r.toString}")
    Some(s) => Success(s, s.length)
  }

def slice[A](p: Parser[A]): Parser[String] = 
  loc: Location => p(loc) match {
    case Success(_, length) => Success(loc.input.slice(length), length)
    case Failure(err) => Failure(err)
  }