package categories.simple

import categories.simple.categoryExamples.*
import categories.simple.functor.*
import Functor.Id
import isEqual.*
import annotations1.*
import zio.prelude.classic

trait Monad0[T[+_]: classic.Functor] {
  def pure[A]: A => T[A]
  def flatten[A]: T[T[A]] => T[A]
  def flatMap[A, B](a: T[A])(f: A => T[B]): T[B] =
  	flatten(zio.prelude.Covariant[T].map(f)(a))
}


// pure: Id ==> T
//
//              pure[A]
//          A ──────────▶ T[A]
//          │               │
//        f │               │ T(f)
//          ▼               ▼
//          B ──────────▶ T[B]
//              pure[B]

//
//
//  flatMap: (T * T) ==> T
//
//                    flatMap
//            T[T[A]] ──────────▶ T[A]
//                │                 │
//          TT(f) │                 │ T(f)
//                ▼                 ▼
//             T[T[B]] ──────────▶ T[B]
//                     flatMap

trait Monad[T[_], X[_, _]: Category]:

  // redefine ==> locally (for endofunctors)
  type ==>[H[_], G[_]] = Nat[H, G, X, X]

  def t       : (X --> X) [T] // endofunctor X --> X
  def pure    : Id ==> T      // 𝜂: 1  ==> T
  def flatten : (T ⊙ T) ==> T // 𝜇: T² ==> T

  @Law
  def unitarity1 =
    (flatten * (pure *: t)) <-> Nat.identity(t)

  @Law
  def unitarity2 =
    (flatten * (t :* pure)) <-> Nat.identity(t)

  @Law
  def associativity =
    (flatten * (t :* flatten)) <-> (flatten * (flatten *: t))

end Monad

object Monad:

  def fromAdjuction
    [C[_,_]: Category, D[_,_]: Category, F[_], G[_]]
    (adj: (F ⊣ G)[C, D])
  : Monad[G ⊙ F, C] =
    import adj.{unit, counit, f, g}
    new Monad {
      def t = g ⊙ f
        : (C --> C) [G ⊙ F]
      def pure = unit
        : Id ==> G ⊙ F
      def flatten = g :* counit *: f
        : G ⊙ F ⊙ G ⊙ F ==> G ⊙ F
    }
end Monad
