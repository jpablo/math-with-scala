package categories.simple.selfType

import annotations1.*
import categories.simple.Isomorphism
import isEqual.{<->, IsEq}

import scala.annotation.targetName


trait Category0:
  type Self
  private type O = Self
  type Hom[A <: O, B <: O]
  type ~>[A <: O, B <: O] = Hom[A, B]

  def id[A <: O]: A ~> A

  extension [A <: O, B <: O, C <: O] (g: B ~> C)
    def ◦ (f: A ~> B): A ~> C = f >>> g

  extension [A <: O, B <: O, C <: O] (f: A ~> B)
    def >>> (g: B ~> C): A ~> C


  @Law
  def associativity[A <: O, B <: O, C <: O, D <: O](
    f: A ~> B,
    g: B ~> C,
    h: C ~> D
  ) =
    h ◦ (g ◦ f) <-> (h ◦ g) ◦ f

  @Law
  def identityR[A <: O, B <: O](f: A ~> B) = f ◦ id[A] <-> f

  @Law
  def identityL[A <: O, B <: O](f: A ~> B) = id[B] ◦ f <-> f

//object Category0:
//  type Aux[C[A, B]] = Category0 { type Self[A, B] =  C[A, B] }
//end Category0


type Scala[A, B] = A => B

given Scala: Category0:
  type Self = Any
  type Hom[A, B] = Scala[A, B]
  def id[A]: A => A = identity[A]
  extension [A, B, C](f: A => B)
    def >>>(g: B ~> C): A => C = f andThen g


class Isomorphism[C, X <: C, Y <: C](
  val c: C is Category0,
  val from: c.Hom[X, Y],
  val to  : c.Hom[Y, X]
):
  // f: X ~> Y is an isomorphism if there exists an arrow g such that
  @Law
  def IdA = (to ◦ from) <-> c.id[X]

  @Law
  def IdB = (from ◦ to) <-> c.id[Y]

end Isomorphism


trait Grupoid extends Category0:
  // A Grupoid is a category where every arrow has an inverse:
  extension [X <: Self, Y <: Self] (f: X ~> Y) def inv: Y ~> X

  // satisfying
  @Law
  def iso[X <: Self, Y <: Self](f: Hom[X, Y]) =
    Isomorphism[Self, X, Y](this, f, f.inv)

