package categories.simple.selfType

import categories.simple.selfType.{*, given}
//import categories.simple.functor.*
import algebra.Homomorphism
import annotations1.*
import categories.simple.selfType.Scala
import categories.simple.categoryExamples.*
import isEqual.*
import zio.prelude.classic

/**
 * A Functor F between the categories C and D
 */
trait Functor (using val S: Category0, val T: Category0):

  type Self[A]

  type ~>[A, B] = S.Self[A, B]
  type ->[A, B] = T.Self[A, B]

  def map[A, B](f: A ~> B): Self[A] -> Self[B]

  // So we can do F(f)
  def apply[A, B](f: A ~> B) = map(f)

  /**
   * Functor composition operator: ⊙
   * Given functors F: S --> T and G: R --> S creates the composite functor
   * G ⊙ F between categories R --> T
   */
//  def ⊙ [G[_], R[_, _]: Category0] (other: (R --> Source)[G]) : (R --> Target) [F ⊙ G] =
//    Functor {
//      [X, Y] => (f: R[X, Y]) => map(other(f))
//    }


//  @Law
//  def composition[X, Y, Z](f: X ~> Y, g: Y ~> Z) =
//    map(g ◦ f) <-> map(g) ◦ map(f)
//
//  @Law
//  def identities[X] =
//    map(S.id[X]) <-> T.id[F[X]]

end Functor

// -------------------
// Functor composition
// -------------------

type ⊙[G[_], F[_]] = [A] =>> G[F[A]]


/**
 * Alias for Functor
 *
 * usage: (C --> D)[F]
 */
//type -->[From[_, _], To[_, _]] =
//  [F[_]] =>> Functor[F, From, To]


object Functor:

  def apply [F[_], C[_, _], D[_, _]] (
    map0: [A, B] => C[A, B] => D[F[A], F[B]]
  )(using S0: Category0.Aux[C], T0: Category0.Aux[D]): Functor =
    new Functor(using S0, T0):
      type Self[A] = F[A]
//      summon[S0.Self[Int, Int] =:= S.Self[Int, Int]]
      def map[A, B](f: S.Self[A, B]): T.Self[F[A], F[B]] = ???
//        map0[A, B](f)

//  type Id[A] = A
//
//  def identity[C[_, _]: Category]: (C --> C) [Id] =
//    Functor {
//      [X, Y] => (f: C[X, Y]) => f
//    }

end Functor

//========================================================================================
/*                                                                                      *
 * contravariant functors                                                               *
 *                                                                                      */
//========================================================================================

//type -/->[C[_, _], D[_, _]] =
//  [F[_]] =>> (Op[C] --> D) [F]
//
//
//type Endofunctor[F[_], C[_, _]] = (C --> C) [F]
//
//// --------------------------------------
//// creates endofunctors from zio.prelude.classic.Functor
//// --------------------------------------
//given [F[+_]](using F: classic.Functor[F]): Endofunctor[F, Scala] =
//  Functor {
//    [A, B] => (f: A => B) => F.map(f)
//  }
//
//object FunctorExamples:
//  summon[Endofunctor[List, Scala]]
//  summon[Endofunctor[Option, Scala]]
//
