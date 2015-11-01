package fpis

import Chap5.MyStream
import org.scalatest.{Matchers, FlatSpec}

class Chap5Test extends FlatSpec with Matchers {
  val stream = MyStream(1,2,3,4,5)

  def from(n: Int): MyStream[Int] =
    if (n == 0)
      MyStream.empty
    else
      MyStream.cons[Int]({ println("exec"); n}, from(n + 1))

  "toList" should "transform a stream to a list" in {
    stream.toList should be(List(1,2,3,4,5))
  }
  "take" should "take the first n elements of a stream" in {
    from(4).take(5)
    from(1).take(2).toList should be(List(1, 2))
    stream.take(0).toList should be(List.empty)
    stream.take(5).toList should be(List(1,2,3,4,5))
  }
  "drop" should "drop the first n elements of a stream" in {
    stream.drop(2).toList should be(List(3,4,5))
  }
}
