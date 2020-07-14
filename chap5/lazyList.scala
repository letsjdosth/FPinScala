sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
        case Empty => None
        case Cons(h,t) => Some(h()) //h(): eval sign
    }

    def toList: List[A] = {
        @annotation.tailrec
        def go[A](st: Stream[A], acc: List[A]): List[A] = st match {
            case Cons(h, t) => go(t(), h() :: acc)
            case Empty => acc
        }
        go(this, Nil).reverse
    }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A] //head, tail. thunk(not evaluated yet) arguments

object Stream {
    def cons[A](hd: =>A, tl: =>Stream[A]): Stream[A] = { // smart constructor (in convention, set name as lower letter word of case class's)
        lazy val head = hd //memoization (evaluated at once(first called time))
        lazy val tail = tl
        Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty // not Empty, but Stream[A] (like this, when the supertype needed for some case class, use smart constructor)

    def apply[A](as: A*): Stream[A] = //constructer
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


    val testSt = Stream(1,2,3)
    testSt.toList
}
//ex5.2~

