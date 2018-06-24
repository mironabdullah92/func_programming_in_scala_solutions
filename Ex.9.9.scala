trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}

// TODO: e-notation
def parseJNumber: Parser[JNumber] =
  (digit.many.slice or char('.')).flatMap match {
    case '.' => digit.many.slice.map(dec => ("0."+dec))
    case int => char('.').flatMap(_ => digit.many.slice.map(dec => int+"."+dec)) or succeed(".0")
  }.map(_.toDouble)

def quote: Parser[Char] =
  char('"')

def parseJString: Parser[JString] = for {
    _ <- quote
    s <- regex("[^\"]".r).many.slice
    _ <- quote
  } yield JString(s)

def parseJBool: Parser[JBool] = for {
  bool <- string("true") or string("false")
} yield JBool(bool.toBoolean)

def parseJSON: Parser[JSON] =
  parseJNumber or parseJBool or parseJString or parseJArray or parseJObject

// todo: error if there is no comma between elems
def parseJArray: Parser[JArray] = {
  def parseElem: Parser[JSON] = for {
    elem <- parseJSON
    _ <- char(',') or success(Nothing)
  } yield elem

  for {
    _ <- char('[')
    list <- parseElem.many
    next <- char(']') or char(',')
  } yield list
}

def parseJObject: Parser[JObject] = {
  def parseRow: Parser[(String, JSON)] = for {
    key <- parseJString
    _ <- char(':')
    value <- parseJSON
  } yield (key -> value)

  for {
    _ <- char('{')
    rows <- parseRow.many
    _ <- char('}')
  } yield rows.toMap
}
