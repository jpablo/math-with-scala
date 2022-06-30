package categories.simple.functor

import categories.simple.{*, given}
import categories.simple.functor.*
import algebra.Homomorphism
import annotations1.*
import categories.simple.Scala
import categories.simple.categoryExamples.*
import isEqual.*
import zio.prelude.classic

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

  // So we can do F(f)
  def apply[A, B](f: A ~> B) = map(f)

  /**
   * Functor composition operator: ⊙
   * Given functors F: S --> T and G: R --> S creates the composite functor
   * G ⊙ F between categories R --> T
   */
  def ⊙ [G[_], R[_, _]: Category] (other: (R --> Source)[G]) : (R --> Target) [F ⊙ G] =
    Functor {
      [X, Y] => (f: R[X, Y]) => map(other(f))
    }


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

  def apply [
    F[_],
    C[_, _]: Category,
    D[_, _]: Category
  ] (
    map0: [A, B] => C[A, B] => D[F[A], F[B]]
  ): Functor[F, C, D] = new:
    def map[A, B](f: A ~> B): F[A] -> F[B] = map0(f)

  type Id[A] = A

  def identity[C[_, _]: Category]: (C --> C) [Id] =
    Functor {
      [X, Y] => (f: C[X, Y]) => f
    }

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
// creates endofunctors from zio.prelude.classic.Functor
// --------------------------------------
given [F[+_]](using F: classic.Functor[F]): Endofunctor[F, Scala] =
  Functor {
    [A, B] => (f: A => B) => F.map(f)
  }

object FunctorExamples:
  summon[Endofunctor[List, Scala]]
  summon[Endofunctor[Option, Scala]]

