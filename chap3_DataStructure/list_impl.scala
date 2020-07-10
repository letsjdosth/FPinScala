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

    //NOT stack-safe
    def foldRight[A,B](as: List[A], z:B)(f: (A,B)=>B): B = {
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }
    }

    def sum2(ns: List[Int]) =
        foldRight(ns, 0)((x,y) => x+y)
    
    def product2(ns: List[Double]) =
        foldRight(ns, 1.0)(_*_)

    def length[A](as: List[A]): Int =
        foldRight(as, 0)((x,y)=>1+y)
    
    //stack-safe
    @annotation.tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B,A)=>B): B = {
        as match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
        }
    }

    def sum3(ns: List[Int]) =
        foldLeft(ns, 0)((x,y) => x+y)
    
    def product3(ns: List[Double]) =
        foldLeft(ns, 1.0)(_*_)

    def length3[A](as: List[A]): Int =
        foldLeft(as, 0)((x,y)=>x+1)

    def reverse[A](as: List[A]): List[A] = {
        foldLeft(as, Nil:List[A])((x,y) => Cons(y,x))
    }
    
    def append3[A](a1: List[A], a2: List[A]): List[A] =
        foldLeft(reverse(a1), a2)((x,y) => Cons(y,x))
    
    def makeOneList[A](lls: List[List[A]]): List[A] = {
        foldLeft(lls, Nil:List[A])(append3)
    }

    def addOne(ls: List[Int]): List[Int] = {
        ls match {
            case Nil => Nil
            case Cons(x,xs) => Cons(x+1, addOne(xs))
        }
    }

    def dbbToStr(ls: List[Double]): List[String] = {
        ls match {
            case Nil=>Nil
            case Cons(x, xs) => Cons(x.toString, dbbToStr(xs))
        }
    }

    def map[A,B](as: List[A])(f: A=>B): List[B] = {
        as match {
            case Nil => Nil
            case Cons(x, xs) => Cons(f(x), map(xs)(f))
        }
    }

    def filter[A](as: List[A])(f: A=>Boolean): List[A] = {
        as match {
            case Nil => Nil
            case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
            case Cons(x, xs) => filter(xs)(f)
        }
    }

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
        as match {
            case Nil => Nil
            case Cons(x, xs) => append3(f(x), flatMap(xs)(f))
        }
    }

    def filterUsingFlatMap[A](as: List[A])(f: A=>Boolean): List[A] = {
        flatMap(as)(a => if (f(a)) List(a) else Nil)
    }

    def addEach(a1 : List[Int], a2: List[Int]): List[Int] = {
        (a1, a2) match {
            case (Nil, Nil) => Nil
            case (Cons(_, _), Nil) => Nil
            case (Nil, Cons(_, _)) => Nil
            case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, addEach(xs, ys))
        }
    }

    def zipWith[A](a1: List[A], a2: List[A])(f: (A,A) => A) : List[A] = {
        (a1, a2) match {
            case (Nil, Nil) => Nil
            case (Cons(_, _), Nil) => Nil
            case (Nil, Cons(_, _)) => Nil
            case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
        }
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

// exercise3.8
List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) //res0: List[Int] = Cons(1,Cons(2,Cons(3,Nil)))

List.length(List(1,2,3,4,5))
List.length(ex3)

List.foldLeft(List(1,2,3,4),0)(_+_)
List.sum3(List(1,2,3,4))
List.product3(List(1,2,3,4))
List.length3(List(1,2,3,4))

List.reverse(List(1,2,3,4))
List.append3(List(1,2,3,4), List(5,6,7,8))
List.makeOneList(List(List(1,2,3,4),List(5,6,7,8),List(9,10)))


List.addOne(List(1,2,3,4))
List.dbbToStr(List(1.0,2.35,3.456,4.1111))
List.map(List(1,2,3,4))(_+2)
List.filter(List(1,2,3,4,5,6,7,8,9))(x=>((x%2)==0))
List.flatMap(List(1,2,3))(i=>List(i,i))
List.filterUsingFlatMap(List(1,2,3,4,5,6,7,8,9))(x=>((x%2)==0))
List.addEach(List(1,2,3), List(4,5,6))
List.zipWith(List(1,2,3), List(4,5,6))(_*_)
List.zipWith(List(1,2,3), List(4,5))(_*_)