package categories.simple.liskov

import categories.simple.Category
// ----------------
// Liskov/Subtypes category
// ----------------


given Subtypes: Category[<:<] with
  def id[A] = <:<.refl
  extension [A, B, C]
    (g: B <:< C) def ◦ (f: A <:< B): A <:< C =
      g compose f


object SubtypesExamples {

  trait A
  trait B extends A
  trait C extends B

  given CanEqual[A, A] = CanEqual.derived
  given CanEqual[A =:= A, A =:= A] = CanEqual.derived
  given CanEqual[A <:< A, A <:< A] = CanEqual.derived

  Subtypes.id[A] == summon[A <:< A]

  import Subtypes.~>

  val f: C ~> B = summon[C <:< B]
  val g: B ~> A = summon[B <:< A]
  val h: C ~> A = summon[C <:< A]

//  g ◦ f == h
}

