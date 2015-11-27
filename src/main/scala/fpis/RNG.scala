package fpis

object RNG {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    if (n == Int.MinValue) (0, nextRNG)
    else if (n < 0) (-n, nextRNG)
    else (n, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n2, rng2) = nonNegativeInt(rng)
    val newDouble = n2.toDouble / Int.MaxValue.toDouble
    (newDouble, rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, nextRNG) = rng.nextInt
    val (d, rng2) = double(nextRNG)
    ((n, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, nextRNG) = double(rng)
    val (n, rng2) = nextRNG.nextInt
    ((d, n), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, nextRNG) = double(rng)
    val (d2, rng2) = double(nextRNG)
    val (d3, rng3) = double(rng2)
    ((d, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def mkList(count: Int, list: List[Int], rng: RNG): (List[Int], RNG) = {
      count match {
        case 0 => (list, rng)
        case _ => {
          val (n, nextRNG) = rng.nextInt
          mkList(count - 1, list ++ List(n), nextRNG)
        }
      }
    }
    val l = List()
    mkList(count, l, rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a,rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def doubleWithMap: Rand[Double] =
    map(nonNegativeInt)(a => a.toDouble / Int.MaxValue.toDouble)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng)
      (f(a,b), rng3)
    }
  }

  def append[B] = (list:List[B], el:B) => list ++ List(el)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldLeft(unit(List[A]()) )( (randList: Rand[List[A]],rand: Rand[A]) => {
      map2(randList, rand)(append)
    } )
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a,rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

}
