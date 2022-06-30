package algebra

import isEqual.*
import annotations1.*

trait Monoid[M]:
  val id: M
  extension (a: M) def * (b: M): M

  @Law
  def associativity(a: M, b: M, c: M) =
    a * (b * c) <-> (a * b) * c

  @Law
  def identityL(a: M) = id * a <-> a

  @Law
  def identityR(a: M) = a * id <-> a

end Monoid


object MonoidInstances:

  def funcMonoid[A] =
    new Monoid[A => A]:
      val id = identity[A]
      extension (a: A => A) def * (b: A => A) = a compose b
      // identities, associativity

  given Monoid[Int] with
    val id = 0
    extension (x: Int) def * (y: Int) = x + y

  given Monoid[String] with
    val id = ""
    extension (x: String) def * (y: String) = x + y

end MonoidInstances
