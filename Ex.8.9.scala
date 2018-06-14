def &&(p: Prop): Prop =
  (n, rng) => {
    val leftResult = run(n, rng)
    if (leftResult.isFalsified)
      leftResult
    else
      p.run(n, rng)
  }

def &&(p: Prop): Prop =
  (n, rng) => {
    val leftResult = run(n, rng)
    if (!leftResult.isFalsified)
      leftResult
    else
      p.run(n, rng)
  }