package fpis

import fpis.Chap6.RNG
import org.scalatest.{FlatSpec, Matchers}

class Chap6Test extends FlatSpec with Matchers {
  case class FakeRNG(seed: Int) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = FakeRNG(seed + 2)
      val n = seed + 2
      (n, newSeed)
    }
  }

  val rng = FakeRNG(0)

  "ints" should "return a list of ints" in {
    Chap6.ints(3)(rng) should be(List(2,4,6), FakeRNG(6))
  }
}
