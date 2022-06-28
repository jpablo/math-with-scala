package categories.simple

// ----------------
// Terminal objects
// ----------------


// An object ⊤ is terminal in a Category C if for every
// object Y there is one and only one arrow from Y to ⊤ in C
trait Terminal[⊤, ~>[_, _]: Category]:
  def arrow[Y]: Y ~> ⊤

  // alias
  def I[Y] = arrow[Y]

// The evidence that a given type A is terminal is an instance
// of the above trait.

// In Scala terminal objects are types with only one inhabitant:

// Example: Unit
given Terminal[Unit, Scala] with
  def arrow[Y] = _ => ()

// Another example: a singleton object is the only inhabitant of a type
object Singleton

given Terminal[Singleton.type, Scala] with {
  def arrow[Y] = _ => Singleton
}

// all terminal objects are isomorphic
def exercise_1[A, B, ~>[_, _]](
  using
    C0 : Category[~>],
    TA: Terminal[A, ~>],
    TB: Terminal[B, ~>],
    eqB: CanEqual[B ~> B, B ~> B],
    eqA: CanEqual[A ~> A, A ~> A],
 ) = {
    new Isomorphism(
      from = TB.arrow[A],
      to = TA.arrow[B]
    ) {
      // since:
      TB.arrow[A] ◦ TA.arrow[B] == C0.id[B]
      // and
      TA.arrow[B] ◦ TB.arrow[A] == C0.id[A]
      // because the Hom[X, X] = { id[X] } for every terminal object B
    }
}