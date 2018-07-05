/**
 * Cant Work because 2 processes are not sync. So
 * zip has to be done manually.
 *  def zip[I,O,O2](p: Process[I,O])(p2: Process[I,O2]): Process[I, (O,O2)] =
 *    for {
 *      i <- Await {
 *          case Halt() => Halt()
 *          case Some(i) => Some(i)
 *        }.repeat
 *      o <- p
 *      o2 <- p2
 *    } yield (o, o2)
 *
 */

def zip[O2](p2: Process[I,O2]): Process[I, (O, O2)] = (this, p2) match {
  case (Halt(), _) => Halt()
  case (_, Halt()) => Halt()
  case (Emit(h1,t1), Emit(h2,t2)) => Emit((h1,h2), t1 zip t2)
  case (Await(recv), _) => Await(oi: Option[I] => recv(oi) zip pass(p2, oi))
  case (_, Await(recv)) => Await(oi: Option[I] => pass(this, oi) zip recv(oi))
}

def pass[I,O](p: Process[I,O], oi: Option[I]): Process[I,O] = p match {
  case Halt() => Halt()
  case Emit(h, t) => Emit(h, pass(t, oi))
  case Await(recv) => recv(oi)
}

def mean: Process[Double,Double] =
  sum zip count map (t => t._1/t._2)