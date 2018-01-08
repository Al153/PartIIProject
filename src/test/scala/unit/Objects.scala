package unit

object Objects {
  val Alice = Person("Alice")
  val Bob = Person("Bob")
  val Charlie = Person("Charlie")
  val David = Person("David")
  val Eve = Person("Eve")
  val Fred = Person("Fred")
  val Georgie = Person("Georgie")
  val Hannah = Person("Hannah")
  val Ian = Person("Ian")
  val Jane = Person("Jane")

  // to be used to test failure cases
  val Zoe = Person("Zoe")

  val Mercedes = Car("Mercedes")
  val Ford = Car("Ford")
  val Bentley = Car("Bentley")
  val VW = Car("VW")

  // Pets
  val fido = Pet("Fido", 1, 60.5, isDog = true)
  val rover = Pet("Rover", 2, 49.5, isDog = true)
  val polly = Pet("Polly", 3, 25.4, isDog = false)
  val leo = Pet("Leo", 18, 29, isDog = false)
  val buster = Pet("Buster", 13, 20, isDog = true)
  val gus = Pet("Gus", 4, 30, isDog = false)
  val fin = Pet("Fin", 4, 28, isDog = false)
  val tufty = Pet("Tufty", 6, 6, isDog = false)
  val tilly = Pet("Tilly", 5, 6, isDog = false)
  val pippa = Pet("Pippa", 12, 45, isDog = true)
  val luna = Pet("Luna", 6, 24.5, isDog = false)
  val nelson = Pet("Nelson", 3, 60.1, isDog = true)
  val lucy = Pet("Lucy", 3, 20, isDog = true)
  val jasper = Pet("Jasper", 12, 45, isDog = true)
}