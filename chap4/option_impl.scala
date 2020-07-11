// package fpinscala.errorhandling


sealed trait Option[+A] {
    def map[B](f: A=>B): Option[B] = {
        this match {
            case Some(x) => Some(f(x))
            case None => None
        }
    }
    def getOrElse[B >: A](default: => B): B = {
        this match {
            case Some(x) => x
            case None => default
        }
    }
    
    def flatmap[B](f: A=> Option[B]): Option[B] = {
        map(f) getOrElse None
    }
    
    def orElse[B >: A](ob: => Option[B]): Option[B] = {
        this map (Some(_)) getOrElse ob
    }
    def filter(f: A => Boolean): Option[A] = {
        if(this map f getOrElse false) this
        else None
    }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


// partial function -> deal it with the option!
def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] = {
    val m: Option[Double] = mean(xs)
    m flatmap (m => mean(xs map (x => math.pow(x - m, 2))))
}

(x:Double => math.pow(x-1, 2))

val exOpNone: Option[Int] = None
val exOp1:Option[Int] = Some(1)
val exOp2:Option[Int] = Some(2)
exOpNone.map(_*2)
exOp1.map(_*2)

exOp1.flatmap()
exOp1.orElse(Some(2))
exOpNone.filter(_==1)
exOp1.filter(_==1)
exOp2.filter(_==1)

val testSeq = Seq(0.0, 1.0, -1.0, 2.0)
val testEmptySeq = Seq()
mean(testSeq)
variance(testSeq)
mean(testEmptySeq)
variance(testEmptySeq)

// structure of variance function
testSeq map (x => math.pow(x, 2)) 
mean(testSeq) map (m => (testSeq map (x => math.pow(x - m, 2))))
mean(testSeq) flatmap (m => mean(testSeq map (x => math.pow(x - m, 2))))

