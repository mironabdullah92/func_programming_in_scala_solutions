def countWords(s: String): Int = {
  def processChunk(str: String): WC =
    if (str.length > 1) {
      val (l, r) = str.splitAt(str.length / 2)
      wcMonoid.op(processChunk(l), processChunk(r))
    } else if (str == " ") {
      Part("", 0, "")
    } else {
      Stub(str)
    }
  
  val end = Part("", 0, "")

  wcMonoid.op(end, wcMonoid.op(processChunk(s), end))
}