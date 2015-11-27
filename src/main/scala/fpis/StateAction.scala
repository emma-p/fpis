package fpis.chap6


object StateAction {

  //  type StateAction[S, +A] = S => (A,S)
  case class StateMonad[S, +A](run: S => (A, S)) {

    def flatMap[B](g: A => StateMonad[S, B]): StateMonad[S, B] = StateMonad({ s =>
      val (a, s2) = this.run(s)
      g(a).run(s2)
    })

    def map[B](f: A => B): StateMonad[S, B] = this.flatMap(a => StateMonad.unit[S,B](f(a)))

    def map2[B, C](sb: StateMonad[S, B])(f: (A, B) => C): StateMonad[S, C] = this.flatMap(a => sb.map(b => f(a, b)))
  }

  object StateMonad {
    def unit[S, A](a: A): StateMonad[S, A] = StateMonad(s => (a, s))

    //from fpinscala answers
    def sequence[A, S](fs: List[StateMonad[S, A]]): StateMonad[S, List[A]] = {
        fs.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
    }
  {

  // State automaton

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    val initialState = StateMonad.unit[Machine, (Int, Int)]((this.candies, this.coins))

    def simulateMachine(inputs: List[Input]): StateMonad[Machine, (Int, Int)] = {
      inputs.foldLeft(initialState)( (acc, current) => (this, current) match {
        // machine out of candy
        case (Machine(_, mcandies, _), _) => acc.run(this)
        // turning the knob on a locked machine
        case (Machine(true, _, _), Turn) => acc.run(this)
        // inserting a coin into an unlocked machine
        case (Machine(false, _, _), Coin) => acc.run(this)
        // turning the knob into an unlocked machine
        case (Machine(false,mcandies,mcoins),Turn) =>
          acc.flatMap(((mcandies - 1, mcoins) => Machine(locked = true, mcandies - 1, mcoins)))
        //          // inserting a coin into a locked machine
        case (Machine(true,mcandies,mcoins),Coin) =>
          acc.flatMap((a,b) => StateMonad.unit(Machine(locked = true, mcandies - 1, mcoins), (mcandies - 1, mcoins)))
      }
      )
    }
  }

  //simulateMachine(List(Coin,Turn,Coin)) if Machine(true, 1,2) should be State(Machine(false,0,4), (0,4))
}
