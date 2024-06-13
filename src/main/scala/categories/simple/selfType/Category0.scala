package categories.simple.selfType

import annotations1.*
import categories.simple.Isomorphism
import isEqual.{<->, IsEq}

import scala.annotation.targetName


trait Category0:
  type Self[_, _]
  type ~>[A, B] = Self[A, B]

  def id[A]: A ~> A

  extension [A, B, C] (g: B ~> C)
    def ◦ (f: A ~> B): A ~> C = f >>> g

  extension [A, B, C] (f: A ~> B)
    def >>> (g: B ~> C): A ~> C


  @Law
  def associativity[A, B, C, D](
    f: A ~> B,
    g: B ~> C,
    h: C ~> D
  ) =
    h ◦ (g ◦ f) <-> (h ◦ g) ◦ f

  @Law
  def identityR[A, B](f: A ~> B) = f ◦ id[A] <-> f

  @Law
  def identityL[A, B](f: A ~> B) = id[B] ◦ f <-> f

object Category0:
  type Aux[C[A, B]] = Category0 { type Self[A, B] =  C[A, B] }
end Category0


type Scala[A, B] = A => B

given Scala: Category0 with
  type Self[A, B] = Scala[A, B]
  def id[A]: A => A = identity[A]
  extension [A, B, C](f: A => B)
    def >>>(g: B ~> C): A => C = f andThen g


class Isomorphism[X, Y, ~~>[_, _]](
  val from: X ~~> Y,
  val to  : Y ~~> X
)(using C: Category0.Aux[~~>]):
  // f: X ~> Y is an isomorphism if there exists an arrow g such that
  @Law
  def IdA = (to ◦ from) <-> C.id[X]

  @Law
  def IdB = (from ◦ to) <-> C.id[Y]


trait Grupoid extends Category0:
  // A Grupoid is a category where every arrow has an inverse:
  extension [X, Y] (f: X ~> Y) def inv: Y ~> X

  // satisfying
//  @Law
//  def iso[X, Y](f: X ~> Y) =
//    Isomorphism(f, f.inv)(using this.Self)

