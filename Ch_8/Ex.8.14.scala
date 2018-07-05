val intGen = Gen.choose(-10000, 10000)

@annotation.tailrec
def isSorted(l: List[Int]): Boolean =
  list.isEmpty || (list.head == l.min && isSorted(l.tail))

val sortedProp = forAll(listOf1(intGen))(isSorted(_.sorted))