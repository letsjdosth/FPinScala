// 주석!
/* 주석! */
/** 문서화 주석! */

// all codes should be in object or class definition

object MyModule {
    def abs(n: Int): Int =
        if (n<0) -n
        else n

    private def formatAbs(x: Int) = {
        val msg = "The absolute value of %d is %d"
        msg.format(x, abs(x))
    }

    def main(args: Array[String]): Unit = //impure. 'main' should be defined following this form.
        println(formatAbs(-42))
}
