package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def size[A](tr: Tree[A]): Int =
        tr match {
            case Leaf(_) => 1
            case Branch(Leaf(a), Leaf(b)) => 2
            case Branch(a, Leaf(b)) => size(a) + 1
            case Branch(Leaf(a), b) => size(b) + 1
            case Branch(a, b) => size(a)+size(b)
        }
    
    def max(tr: Tree[Int]): Int = 
        tr match {
            case Leaf(a) => a
            case Branch(Leaf(a),Leaf(b)) => a max b
            case Branch(a, Leaf(b)) => max(a) max b
            case Branch(Leaf(a), b) => max(b) max a
            case Branch(a, b) => max(a) max max(b)
        }

    def depth[A](tr: Tree[A]): Int = 
        tr match {
            case Leaf(a) => 1
            case Branch(Leaf(a),Leaf(b)) => 2
            case Branch(a, Leaf(b)) => depth(a) + 1
            case Branch(Leaf(a), b) => depth(b) + 1
            case Branch(a, b) => (depth(a) max depth(b)) + 1
        }
    
    def map[A](tr: Tree[A])(f: A=>A): Tree[A] = 
        tr match {
            case Leaf(a) => Leaf(f(a))
            case Branch(Leaf(a),Leaf(b)) => Branch(Leaf(f(a)), Leaf(f(b)))
            case Branch(a, Leaf(b)) => Branch(map(a)(f), Leaf(f(b)))
            case Branch(Leaf(a), b) => Branch(Leaf(f(a)), map(b)(f))
            case Branch(a, b) => Branch(map(a)(f), map(b)(f))
        }
    
    def fold[A,B](tr: Tree[A], z: B)(f: (A,B)=>B): B = 
        tr match {
            case Leaf(a) => f(a, z)
            case Branch(Leaf(a),Leaf(b)) => f(a, f(b, z))
            case Branch(a, Leaf(b)) => f(b, fold(a,z)(f))
            case Branch(Leaf(a), b) => f(a, fold(b,z)(f))
            case Branch(a, b) => fold(a, fold(b,z)(f))(f)
        }

    def maxfold(tr: Tree[Int]): Int = 
        fold(tr,0)((x,y) => x max y) //not 0, but -inf
    
    def sizefold[A](tr:Tree[A]): Int = 
        fold(tr,0)((x,y) => y+1)

    //Note: Answer - see the signiture!
    def foldAnswer[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
        case Leaf(a) => f(a)
        case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

    }
}


val exStrTree: Tree[String] = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
val exIntTree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(5)), Branch(Leaf(4), Leaf(2)))
val exIntTree2: Tree[Int] = Branch(Branch(Leaf(1), Leaf(5)), Branch(Branch(Leaf(4),Leaf(8)), Leaf(2)))
Tree.size(exStrTree)
Tree.max(exIntTree)

Tree.size(exIntTree2)
Tree.max(exIntTree2)
Tree.depth(exIntTree2)
Tree.map(exIntTree2)(_*2)

Tree.sizefold(exIntTree2)
Tree.maxfold(exIntTree2)
