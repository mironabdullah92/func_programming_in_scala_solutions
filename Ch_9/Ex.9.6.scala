def numbLetters(p: Parser[String]): Parser[String] = {
  many(regex("0-9".r)).slice.flatMap(nStr => {
    val n = nStr.toInt
    listOfN(n, p).map(_ => n)
  })
}