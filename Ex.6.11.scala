sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

def processInput(in: Input)(m: Machine): Machine =
  (in, m) match {
    case (Coin, Machine(true, candies, coins)) if candies > 0 => 
      Machine(false, candies, coins+1)
    case (Turn, Machine(false, candies, coins)) =>
      Machine(true, candies-1, coins)
    case _ => m
  }

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
  _ <- State.sequence(inputs map(in => State.modify(processInput(in) _)))
  m <- State.get
} yield (m.candies, m.coins)