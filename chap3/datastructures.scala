package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A] //+A: covariant

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x,xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = //A*: 0~n arguments permitted (same as python)
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    
    def tail[A](ls: List[A]): List[A] = ls match {
        case Nil => Nil
        case Cons(_, x) => x
    }


    def drop[A](n: Int, ls: List[A]): List[A] ={
        // @annotation.tailrec
        def go(n: Int, ls: List[A]): List[A] =
            if (n==0) ls
            else go(n-1, tail(ls))
        go(n, ls)
    }

    def dropWhile[A](ls: List[A])(f: A => Boolean): List[A] = {
        ls match{
            case Cons(h,t) if f(h) => dropWhile(t)(f)
            case _ => ls
        }
    }

    def setHead[A](head: A, ls: List[A]): List[A] =
        Cons(head, tail(ls))
    
    def append[A](a1: List[A], a2: List[A]): List[A] =
        a1 match {
            case Nil => a2
            case Cons(h,t) => Cons(h, append(t, a2))
        }

    //inefficient
    def init[A](ls: List[A]): List[A] = {
        def go(ls: List[A], newls: List[A]):List[A] = {
            if (ls==Nil) newls    
            else {
                val nextls: List[A] = ls match {
                case Cons(y, Nil) => newls
                case Cons(y, _) => append(newls, List(y))
                }
                go(tail(ls), nextls)
            }
        }
        go(ls, Nil: List[A])
    }
}


// USE REPL
// assign
val ex1: List[Double] = Nil
val ex2: List[Int] = Cons(1, Nil)
val ex3: List[String] = Cons("A", Cons("B", Nil))
val exApply = List(1,2,3)
val exApply2 = List(1,2,3,6,8,9,10,11)

// match
val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4,_))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4,_)))) => x+y
    case _ => 101
} //3


//test methods
List.sum(exApply) //6

List.tail(exApply)
List.tail(ex1)

List.setHead(10, exApply)
List.setHead("C", ex3)

List.drop(2, exApply)

List.dropWhile(exApply2)((x:Int) => (x==8))
List.dropWhile(ex2)(x => (x==8))
List.dropWhile(ex1)((x:Double) => (x==8.0))

List.init(exApply)

//~3.3