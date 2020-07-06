// idea: 청구건을 하나의 값으로 돌려주도록 하자.
// 생성문제만 다루고, 추가 처리문제는 바깥에서 해결하도록 빼자.
// 'separation of concern' (관심사(문제) 분리)

class Cafe{
    def buyCoffee(cc: CreditCard) : (Coffee, Charge) = {
        val cup = new Coffee()
        (cup, Charge(cc, cup.price))
    }

    def buyCoffees(cc: CreditCard, n:Int) : (List[Coffee], Charge) = {
        val purchases : List[(Coffee, Charge)] = list.fill(n)(buyCoffee(cc))
        val (coffees, charges) = purchases.unzip
        (coffees, charges.reduce((c1,c2) => c1.combine(c2)))
    }
}

case class Charge(cc: CreditCard, amount : Double) {
    def combine(other: Charge) : Charge = 
    if (cc == other.cc)
        Charge(cc, amount + other.amount)
    else
        throw new Exception("Cant combine charges to different cards")
}

def coalesce(charges: List[Charge]) : List[Charge] = 
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList