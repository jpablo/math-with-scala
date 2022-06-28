package categories.typeClasses

import discipline1.*
import algebra.Homomorphism

/**
 *  A Category where types can have a contraint of the
 *  form P[A]
 * @tparam P A type class constraint
 * @tparam Hom
 */
trait CategoryTC[P[_], Hom[_, _]]:
  type ~>[A, B] = Hom[A, B]
  def id[A: P]: A ~> A
  extension [A: P, B: P, C: P]
    (g: B ~> C) def ◦ (f: A ~> B): A ~> C

type Sub[U] = [A] =>> A <:< U

type CategorySub[U, Hom[_, _]] =
  CategoryTC[Sub[U], Hom]

// ---------------------------
// Generic algebraic structure
// ---------------------------

def algCat[P[_]] =
  type Hom[A, B] = Homomorphism[P, A, B]
  new CategoryTC[P, Hom]:
    def id[A: P] = Homomorphism(identity)
    extension [A: P, B: P, C: P] (g: B ~> C) def ◦ (f: A ~> B): A ~> C =
      Homomorphism(g compose f)

// ---------------------------------------------------------
// Mon: the category of all Monoids and monoid homomorphisms
// ---------------------------------------------------------
import cats.Group
import cats.implicits.*
import scala.language.implicitConversions

val Grp = algCat[Group]

object GroupHomLaws {
  def multiplication[A: Group, B: Group](
    f: Homomorphism[Group, A, B],
    x: A,
    y: A
  )(using CanEqual[B, B]) =
    f(x |+| y) == (f(x) |+| f(y))
}

// --------------------------------------------------------
// Ord, category of ordereded sets and increasing mappings
// --------------------------------------------------------

import sets.Orders.Order

val Ord = algCat[Order]

object OrdLaws {
  def monotonic[A: Order, B: Order](
    f: Homomorphism[Order, A, B],
    x: A,
    y: A
  ) =
    if (x <= y) then f(x) <= f(y) else true
}
