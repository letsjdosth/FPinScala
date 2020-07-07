object ParamPolyExample {
    // parametric polymorphism
    def findFirst[A](as: Array[A], p: A=>Boolean): Int = {
        @annotation.tailrec
        def loop(n: Int): Int =
            if (n>= as.length) -1
            else if (p(as(n))) n
            else loop(n+1)

        loop(0)
    }

    def isSorted[A](as: Array[A], ordered: (A,A)=>Boolean): Boolean = {
        @annotation.tailrec
        def loop(n:Int): Boolean =
            if (n>= as.length-1) true
            else if (ordered(as(n), as(n+1))) loop(n+1)
            else false
        
        loop(0)
    }

    def main(args: Array[String]): Unit = {
        println(findFirst(Array(7,9,13), (x:Int)=>x==9))
        println(findFirst(Array(7,9,13), (x:Int)=>x==13))
        println(isSorted(Array(7,9,13), (x:Int, y:Int)=>x<y))
        println(isSorted(Array(7,9,6), (x:Int, y:Int)=>x<y))
    }
}