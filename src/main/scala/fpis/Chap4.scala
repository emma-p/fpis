package fpis

object Chap4 {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) scala.None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => {
      mean(xs.map(x => math.pow(x - m, 2.0)))
    })
  }
}
