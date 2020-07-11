
def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

def Try[A](a: => A): Option[A] = //lazy eval
    try Some(a)
    catch { case e: Exception => None }

def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B)=>C): Option[C] = {
    (a,b) match {
        case ((Some(ax),Some(bx))) => Try(f(ax, bx))
        case _ => None
    }
}

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = 1.0

def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
}

val testAge1: String = "1"
val testTkt1: String = "1"
parseInsuranceRateQuote(testAge1, testTkt1)

val testAge2: String = "hi"
val testTkt2: String = "2"
parseInsuranceRateQuote(testAge2, testTkt2)



def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
        case Nil => Some(Nil)
        case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

//inefficient
def parseInt(a: List[String]): Option[List[Int]] = 
    sequence(a map (i => Try(i.toInt)))

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
        case Nil => Some(Nil)
        case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }
}


val testList = List(Option(1), Option(2), Option(3), None)
val testList2 = List(Option(1), Option(2), Option(3))
testList map (x => x getOrElse None)
sequence(testList)
sequence(testList2)

val testStrNumList: List[String] = List("1","3","2")
traverse(testStrNumList)(x => Try(x.toInt))
