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

    def take(n: Int): Stream[A] = this match {
        case Cons(h, t) if n>1 => Stream.cons(h(), t().take(n-1))
        case Cons(h, _) if n==1 => Stream.cons(h(), Stream.empty)
        case Empty => Stream.empty
    }
        
    def drop(n: Int): Stream[A] = {
        @annotation.tailrec
        def go[A](st: Stream[A], n: Int): Stream[A] =
            if (n==0) st else {
                st match {
                    case Cons(h,t) => go(t(), n-1)
                    case Empty => Empty
                }
            }
        go(this, n)
    }

    def takeWhile(p: A=> Boolean): Stream[A] = this match {
        case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
        case _ => Stream.empty
    }

    def exists(p: A=> Boolean): Boolean = this match {
        case Cons(h,t) => p(h()) || t().exists(p) //||: lazy val
        case _ => false
    }

    def foldRight[B](z: =>B)(f: (A, =>B) => B): B = this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    def existsWithFR(p: A=> Boolean): Boolean =
        foldRight(false)((h,t)=>p(h)||t)

    def forAll(p: A=> Boolean): Boolean =
        foldRight(true)((h,t)=> p(h) && t)

    def takeWhileWithFR(p: A=> Boolean): Stream[A] = {
        foldRight(Stream.empty[A])((h,t) => if (p(h)) Stream.cons(h, t) else Stream.empty)
    }

    def headOptionWithFR: Option[A] =
        foldRight(None: Option[A])((h,_) => Some(h))

    def map[B](f: A => B): Stream[B] =
        foldRight(Stream.empty[B])((h,t) => Stream.cons(f(h), t))

    def filter(f: A => Boolean): Stream[A] =
        foldRight(Stream.empty[A])((h,t) => if (f(h)) Stream.cons(h, t) else t)
    
    def append[B>:A](s: => Stream[B]): Stream[B] =
        foldRight(s)((h,t) => Stream.cons(h,t))
    
    def flatMap[B](f: A => Stream[B]): Stream[B] =
        foldRight(Stream.empty[B])((h,t) => f(h).append(t))

    def find(p: A => Boolean): Option[A] =
        filter(p).headOption //first class loop. if find, finish the loop immediately.

    def mapWithUnfold[B](f: A => B): Stream[B] =
        Stream.unfold(this){
            case Cons(h,t) => Some(f(h()), t())
            case Empty => None
        }
    def takeWithUnfold(n: Int): Stream[A] = 
        Stream.unfold((this,n)){
            case (Cons(h,t),n) if n>1 => Some(h(), (t(),n-1))
            case (Cons(h,_),n) if n==1 => Some(h(), (Stream.empty,0))
            case (Empty,_) => None
        }

    def takeWhileWithUnfold(p: A=> Boolean): Stream[A] = 
        Stream.unfold(this){
            case Cons(h,t) if p(h()) => Some(h(), t())
            case _ => None
        }

    def zipWithWith_withUnfold[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = {
        Stream.unfold((this, s2)){
            case (Cons(h1,t1), Cons(h2,t2)) => Some(f(h1(),h2()), (t1(),t2()))
            case _ => None
        }
    }
    def zipAllWithUnfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
        Stream.unfold((this, s2)){
            case (Cons(h1,t1), Cons(h2,t2)) => Some((Some(h1()),Some(h2())), (t1(),t2()))
            case (Cons(h1,t1), Empty) => Some((Some(h1()), None), (t1(),Empty))
            case (Empty, Cons(h2,t2)) => Some((None,Some(h2())), (Empty,t2()))
            case (Empty, Empty) => None

        }
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

    val ones: Stream[Int] = cons(1, ones)
    
    def constant[A](a: A): Stream[A] =
        cons(a, constant(a))

    def from(n: Int): Stream[Int] =
        cons(n, from(n+1))

    def fibs: Stream[Int] = {
        def go(f0: Int, f1: Int): Stream[Int] =
            cons(f0, go(f1, f0+f1))
        go(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A,S)]): Stream[A] = { //corecursive function
        f(z) match {
            case Some((h,s)) => cons(h, unfold(s)(f))
            case None => empty
        }
    }

    def fibsWithUnfold: Stream[Int] = {
        unfold((0,1)){case (f0, f1) => Some(f0, (f1, f0+f1)): Option[(Int, (Int, Int))]}
    }
    
    def fromWithUnfold(n: Int): Stream[Int] = {
        unfold(n)(x => Some(x, x+1): Option[(Int, Int)])
    }

    def constantWithUnfold[A](a: A): Stream[A] =
        unfold(a)(x => Some(x, x): Option[(A,A)])
    
    def onesWithUnfold: Stream[Int] = constantWithUnfold(1)
}

val testSt = Stream(1,2,3)
testSt.toList
testSt.take(2).toList
testSt.drop(2).toList
testSt.takeWhile(_<3).toList
testSt.forAll(_==1)
testSt.existsWithFR(_==1)
testSt.takeWhileWithFR(_%2==1).toList
testSt.headOptionWithFR
testSt.map(_*2).toList
testSt.filter(_>1).toList
testSt.append(Stream(4,5)).toList
testSt.flatMap(a=>Stream(a,0)).toList

testSt.map(_+10).filter(_%2==0).toList // first-class loop

 //infinite stream
val ones = Stream.ones
ones.take(5).toList
ones.exists(_%2 !=0)
ones.map(_+1).exists(_%2==0)
ones.takeWhile(_==1)
ones.forAll(_!=1)

val twos = Stream.constant(2)
twos.take(2).toList

val stInt = Stream.from(1)
stInt.take(5).toList

val stFib = Stream.fibs
stFib.take(10).toList

Stream.fromWithUnfold(5).take(5).toList

// ~5.13