package categories.simple.functor

import categories.simple.{*, given}
import categories.simple.functor.*
import algebra.Homomorphism
import annotations1.*
import categories.simple.Scala
import categories.simple.categoryExamples.*
import isEqual.*

/**
 * A Functor F between the categories C and D
 */
trait Functor
  [F[_], Source[_, _], Target[_, _]]
  (using
    S: Category[Source],
    T: Category[Target]
  ):

  type ~>[A, B] = Source[A, B]
  type ->[A, B] = Target[A, B]

  def map[A, B](f: A ~> B): F[A] -> F[B]
  // helper method to be able to do F(f)
  def apply[A, B](f: A ~> B) = map(f)

  /**
   * Functor composition operator: ⊙
   * Given functors G: D --> E and F: C --> D creates the composite functor
   * G ⊙ F between categories C --> E
   */
  def ⊙ [F0[_], C[_, _]: Category] (F0: (C --> Source)[F0]) : (C --> Target) [F ⊙ F0] =
    Functor( [X, Y] => (f: C[X, Y]) => map(F0(f)) )

  @Law
  def composition[X, Y, Z](f: X ~> Y, g: Y ~> Z) =
    map(g ◦ f) <-> map(g) ◦ map(f)

  @Law
  def identities[X] =
    map(S.id[X]) <-> T.id[F[X]]

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
type -->[From[_, _], To[_, _]] =
  [F[_]] =>> Functor[F, From, To]


object Functor:
  // Static constructor
  def apply
    [F[_], C[_, _]: Category, D[_, _]: Category]
    (map0: [A, B] => C[A, B] => D[F[A], F[B]]) =
      new Functor[F, C, D] { def map[A, B](f: A ~> B): F[A] -> F[B] = map0(f) }

  type Id[A] = A

  def identity[C[_, _]: Category]: (C --> C) [Id] =
    Functor( [X, Y] => (f: C[X, Y]) => f )

end Functor

//========================================================================================
/*                                                                                      *
 * contravariant functors                                                               *
 *                                                                                      */
//========================================================================================

type -/->[C[_, _], D[_, _]] =
  [F[_]] =>> (Op[C] --> D) [F]


type Endofunctor[F[_], C[_, _]] = (C --> C) [F]

// --------------------------------------
// creates endofunctors from cats.Functor
// --------------------------------------
given [F[+_]](using F: zio.prelude.classic.Functor[F]): Endofunctor[F, Scala] with
  def map[A, B](f: A => B) = F.map(f)

object FunctorExamples:
  summon[Endofunctor[List, Scala]]
  summon[Endofunctor[Option, Scala]]

