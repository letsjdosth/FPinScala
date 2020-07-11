def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail")
    try {
        val x = 42 + 5
        x + y
    } catch {
        case e: Exception => 43
    }
}

failingFn(1)
// java.lang.Exception: fail
//   at .failingFn(<console>:2)




def failingFn2(i: Int): Int = {
    try {
        val x = 42 + 5
        x + ((throw new Exception("fail")):Int)
    } catch {
        case e: Exception => 43
    }
}

failingFn2(1)
// Int = 43
// The exception Instant is not referentially transparent, or dependent on a context.
