def intDouble(rng: RNG): ((Int,Double), RNG) = {
  val (i,newRng1) = rng.nextInt
  val (d,newRng2) = double(newRng1)
  ((i,d), newRng2)
}

def doubleInt(rng: RNG): ((Double,Int), RNG) = {
  val (d,newRng1) = double(rng)
  val (i,newRng2) = newRng1.nextInt
  ((d,i), newRng2)
}

def double3(rng: RNG): ((Double,Double,Double), RNG) = {
  val (d1,newRng1) = double(rng)
  val (d2,newRng2) = double(newRng1)
  val (d3,newRng3) = double(newRng2)
  ((d1,d2,d3), newRng3)
}