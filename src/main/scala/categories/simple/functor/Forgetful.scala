package categories.simple.functor

import categories.simple.{_, given}
import categories.simple.functor.*
import algebra.Homomorphism
import annotations1.*
import categories.simple.categoryExamples.*
import isEqual.*
import Functor.Id
//========================================================================================
/*                                                                                      *
 * forgetful functor                                                                    *
 *                                                                                      */
//========================================================================================

import categories.typeClasses.CategoryTC

trait FunctorTC[F[_], P[_], C[_, _], Q[_], D[_, _]](
  using
    CategoryTC[P, C],
    CategoryTC[Q, D]
  ) {
  type ~>[A, B] = C[A, B]
  type ->[A, B] = D[A, B]

  def map[A: P, B: P](f: A ~> B): F[A] -> F[B]
}

val ScalaTC: CategoryTC[Id, Scala] =
  new CategoryTC:
    def id[A: Id] = Scala.id[A]
    extension [A: Id, B: Id, C: Id] (g: B => C) def ◦ (f: A => B) =
      g compose f


def forgetful[P[_], C[X, Y] <: Homomorphism[P, X, Y]](
    using
      CategoryTC[P,  C]
  ): FunctorTC[Id, P, C, Id, Scala] = {

  given CategoryTC[Id,  Scala] = ScalaTC

  new FunctorTC {
    def map [A: P, B: P](f: C[A, B]): A => B = f
  }
}
