package fpis

object Chap5 {
  sealed trait MyStream[+A] {
    def toList: List[A] = this match {
      case Empty => List.empty
      case Cons(h, tl) => h() :: tl().toList
    }

    def take(n: Int): MyStream[A] = this match {
      case Empty => MyStream.empty
      case _ if n == 0 => MyStream.empty
      case Cons(h,tl) if n >= 1 => {
        if (n == 1)
          MyStream.cons(h(), MyStream.empty)
        else MyStream.cons(h(), tl().take(n - 1))
      }
    }

    def drop(n: Int): MyStream[A] = ???
  }

  case object Empty extends MyStream[Nothing]
  case class Cons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

  object MyStream {
    def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: MyStream[A] = Empty

    def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  }
}
