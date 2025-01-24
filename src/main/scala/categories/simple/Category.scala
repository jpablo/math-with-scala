package categories.simple

import isEqual.*
import annotations1.*
import scala.annotation.targetName


trait CategoryS[U, Hom[_ <: U, _ <: U]]:
  type ~>[A <: U, B <: U] = Hom[A, B]
  type <~[A <: U, B <: U] = Hom[B, A]

  // Arrow constructor
  def id[A <: U]: A ~> A

  // "compose" operator
  extension [A <: U, B <: U, C <: U](g: B ~> C)
    def ◦ (f: A ~> B): A ~> C

  // andThen operator
  extension [A <: U, B <: U, C <: U](f: A ~> B)
    def >>> (g: B ~> C): A ~> C = g ◦ f

  @Law
  def associativity[A <: U, B <: U, C <: U, D <: U](
    f: A ~> B,
    g: B ~> C,
    h: C ~> D
  ) =
    h ◦ (g ◦ f) <-> (h ◦ g) ◦ f

  @Law
  def identityL[A <: U, B <: U](f: A ~> B) =
    (id[B] ◦ f) <-> f

  @Law
  def identityR[A <: U, B <: U](f: A ~> B) =
    f ◦ id[A] <-> f

end CategoryS


type Category[Hom[_, _]] = CategoryS[Any, Hom]


object Category:
  def apply[C[_, _]](using C: Category[C]) = C


//========================================================================================
/*                                                                                      *
 * Category of types and pure functions                                                 *
 *                                                                                      */
//========================================================================================
type Scala[A, B] = A => B

given Scala: Category[Scala]:
  def id[A]: A => A = identity[A]

  extension [A, B, C](g: B => C)
    def ◦ (f: A => B) = g compose f


//======================
//  Opposite category
//======================

type Op[C[_, _]] = [A, B] =>> C[B, A]

given opInstance: [Hom[_, _]: Category as cat] => Category[Op[Hom]]:
  def id[A] = cat.id[A]

  extension [A, B, C](g: C <~ B)
    def ◦ (f: B <~ A) = cat.◦(f)(g)

