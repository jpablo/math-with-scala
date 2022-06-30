package categories.highOrder

import categories.simple.functor.*
import categories.simple.{given, *}
import categories.simple.Scala

// ----------------------------------------------------------
// A monad is just a monoid in the category of endofunctors.
//
// The category of endofunctors and natural transformations
// ----------------------------------------------------------


// Given a Category[X] creates the category of endofunctors on X

def endo[X[_, _]: Category] =

  type Hom[H[_], G[_]] = (H ==> G)[X, X]

  type EndoX[F[_]] = Endofunctor[F, X]

  new CategoryF[EndoX, Hom]:

    def id[F[_]](using f: EndoX[F]): F ~> F =
      Nat.identity(f)

    extension [F[_]: EndoX, G[_]: EndoX, H[_]: EndoX]
      (m: F ~> G) override def >>> (n: G ~> H): F ~> H =
        n * m

object ExamplesEndo {
//  endo[Scala].id[List]

  // val optionKleisliEndo = endo[[A, B] =>> categories.simple.categoryExamples.Kleisli[Option, A, B]]
}

// -------
// Functor
// -------

//trait FunctorTC1[F[G[_]] <: F2F[G[_]], PC <: F2T, PD <: F2T, C <: Hom2, D <: Hom2](
//  using
//    CategoryTC1[PC, C],
//    CategoryTC1[PD, D]
//  ) {
//  type ~> = C
//  type ⟿ = D
//
//  def map  [A[_], B[_]](f: A ~> B): F[A] ⟿ F[B]
//  def apply[A[_], B[_]](f: A ~> B) = map(f)
//}

// type BifunctorTC1[F[_, _], Prod  <: Hom2, D  <: Hom2] = FunctorTC1[[A] =>> F[Fst[A],  Snd[A]], Prod, D]

// X =:= (F[_], G[_])
// ([A] =>> F[A], [B] =>> G[B])

//type Fst1[X[_]] = [A] =>> X match { case (a, _) => a }
//type Snd1[X[_]] = [A] =>> X match { case (_, b) => b }
//

//type ××[~> <: Hom2, -> <: Hom2] = [A[_], B[_]]  =>> (Fst1[A] ~> Fst1[B], Snd1[A] -> Snd1[B])

// trait BifunctorTC1[F[_, _], C1 <: Hom2, C2 <: Hom2, D <: Hom2] //(using CategoryTC1[C1 × C2], CategoryTC1[D])
//   extends Functor[[A] =>> F[Fst[A],  Snd[A]], C1 ×× C2, D]
