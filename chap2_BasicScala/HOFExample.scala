
object HOFExample {
    // higher order function
    def abs(n: Int): Int =
        if (n<0) -n
        else n

    def factorial(n: Int) = {
        @annotation.tailrec
        def go(n: Int, acc: Int): Int =
            if (n<=0) acc
            else go(n-1, n*acc)
        
        go(n, 1) //use recursion instead of loop! (The scala compiler optimizes this 'tail position self-recursion')
    }

    def fibonacci(n: Int):Int = {
        @annotation.tailrec
        def go(n: Int, last: Int, next: Int): Int = {
            if (n==1) last
            else go(n-1, next, last+next)
        }
        go(n, 0, 1)
    }

    private def formatResult(name: String, n: Int, f: Int=>Int) = {
        val msg = "The %s of %d is %d."
        msg.format(name, n, f(n))
    }

    def main(args: Array[String]): Unit = //impure. 'main' should be defined following this form.
        println(formatResult("absolute value", -42, abs))
        println(formatResult("factorial", 7, factorial))
        println(formatResult("Fibonacci number", 5, fibonacci))
}
