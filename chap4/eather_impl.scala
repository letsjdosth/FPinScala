sealed trait Either [+E, +A] {
    def map[B](f: A => B): Either[E, B] =
        this match {
            case Right(a) => Right(f(a))
            case Left(e) => Left(e)
        }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
        this match {
            case Right(a) => f(a)
            case Left(e) => Left(e)
        }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
        this match {
            case Right(a) => Right(a)
            case Left(e) => b
        }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] =
        (this, b) match {
            case (Right(aa), Right(bb)) => Right(f(aa, bb))
            case (Left(e), _) => Left(e)
            case (Right(aa), Left(e)) => Left(e)
        }
   
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty)
        Left("mean of empty list!")
    else
        Right(xs.sum / xs.length)

def saveDiv(x: Int, y:Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e)}

mean(IndexedSeq())
mean(IndexedSeq(0.5, -0.5))

saveDiv(1, 1)
saveDiv(1, 0) //Either[Exception,Int] = Left(java.lang.ArithmeticException: / by zero)

def Try[A](a: =>A): Either[Exception, A] =
    try Right(a)
    catch {case e: Exception => Left(e)}

// test methods
val testRgt: Either[String, Int] = Right(1)
val testErr: Either[String, Int] = Left("error for test!")
testRgt.map(_*2) //Right(2)
testRgt.flatMap((x: Int) => Right(x*0.5): Either[String, Double]) //Right(0.5)
testRgt.orElse(Right(2)) //Right(1)
testRgt.map2(Right(2))((x,y)=>x+y) //Right(3)
testErr.map(_*2)
testErr.flatMap((x: Int) => Right(x*0.5): Either[String, Double])
testErr.orElse(Right(2)) //Right(2)
testErr.map2(Right(2))((x,y)=>x+y)

// ex1 : with for comprehension
def parseInsuranceRateQuote(
    age: String,
    numberOfSpeedingTickets: String): Either[Exception, Double] =
    for {
        a <- Try(age.toInt)
        tickets <- Try(numberOfSpeedingTickets.toInt)
    } yield insuranceRateQuote(a, tickets)


def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es match {
        case Nil => Right(Nil)
        case Right(h) :: t =>
            Right(h) flatMap (hh => sequence(t) map (hh :: _))
        case Left(e) :: t =>
            Left(e)

    }    
}
val testRgtList: List[Either[String, Int]] = List(Right(1),Right(2),Right(3))
val testErrList: List[Either[String, Int]] = List(Right(1), Left("error"), Left("error2"))
sequence(testRgtList)
sequence(testErrList)


def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
        case Nil => Right(Nil)
        case a :: t => f(a).map2(traverse(t)(f))((x,y)=>x :: y)
    }
}

val testList: List[Double] = List(1,2,3)
val testList2: List[Int] = List(1,0)
traverse(testList)(x => try { Right(1/x) } catch {case e: Exception => Left(e)}) : Either[Exception, List[Double]]
traverse(testList2)(x => try { Right(1/x) } catch {case e: Exception => Left(e)}) : Either[Exception, List[Double]]


// ex2
case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty")
    else Right(new Name(name))

def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range")
    else Right(new Age(age))

def mkPerson(name: String, age: Int): Either [String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

// ex4.8: paste two strings?