package _00_domain

case class Insurance(name: String)
case class Car(insurance: Option[Insurance])
case class Person(car: Option[Car])

val nonDriver = Person(car = None)
val uninsured = Person(Some(Car(insurance = None)))
val insured = Person(Some(Car(Some(Insurance("Acme")))))

val car: Person => Option[Car] =
  person => person.car

val insurance: Car => Option[Insurance] =
  car => car.insurance

/////////////////////////////////////////////////////////////////

def toChars: String => List[Char] = _.toList
def toAscii: Char => List[Char] = _.toInt.toString.toList

@main def mainDomain: Unit =

  assert(toChars("AB") == List('A','B'))
  assert(toAscii('A') == List('6','5'))