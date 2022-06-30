package categories.simple.monoidalCategories

import categories.simple.*
import categories.simple.functor.*
import categories.simple.categoryExamples.{_, given}

/**
 * A Cartesian Monoidal Category with a common bifunctor as both product and tensor
 * @tparam C The category morphisms
 * @tparam F A functor that serves for both product and tensor
 */
trait CartesianMonoidalCategory[C[_, _]: Category, F[_, _]]
  extends MonoidalCategory[C] with InternalProduct[C]:
  type ⨂[A, B] = F[A, B]
  type *[A, B] = F[A, B]

// Any category with products can be made monoidal in a canonical way
// TODO: Finish
class CartesianMonoidalCategory1[C[_, _]](
  using
    val C: Category[C],
    val P: InternalProduct[C]
) extends MonoidalCategory[C] {
  import P.{`*`, T, fst, snd}

  type ⨂[A, B] = A * B
  // or
  type Prod[T] = Fst[T] * Snd[T]

  type I = T

  // Arrows in the product categories
  type C2[A, B] = Prod2[C, C   ][A, B]
  type C3[A, B] = Prod3[C, C, C][A, B]
  // =:=
  //  ( Fst[A] ~> Fst[B],
  //    Snd[A] ~> Snd[B] )

  val tensor: Bifunctor[⨂, C2, C] =
    Functor {
      [A, B] => (f : C2[A, B]) => ???
    }


  // This follows
  val associator: (Lassoc <===> Rassoc)[C3, C] =
    new NaturalIsomorphism with (Lassoc ==> Rassoc)[C3, C] {
      // (x ⨂ y) ⨂ z =:= (x * y) * z
      val source: (C3 --> C)[Lassoc] =
        Functor {
          [A, B] => (f: C3[A, B]) => (f._1 ** f._2) ** f._3
        }

      // x ⨂ (y ⨂ z) =:= (x * (y * z))
      val target: (C3 --> C)[Rassoc] =
        Functor {
          [A, B] => (f: C3[A, B]) => f._1 ** (f._2 ** f._3)
        }

      // (X * Y) * Z ~> X * (Y * Z)
      def apply[X]: Lassoc[X] ~> Rassoc[X] =
        type X1 = Fst[X]
        type X2 = Snd[X]
        type X3 = Third[X]

        val a: Lassoc[X] ~> X1 = fst[X1 * X2, X3] >>> fst[X1, X2]
        val b: Lassoc[X] ~> X2 = fst[X1 * X2, X3] >>> snd[X1, X2]
        val c: Lassoc[X] ~> X3 = snd[X1 * X2, X3]

        val from: Lassoc[X] ~> Rassoc[X] = a * (b * c)
        from

      // X * (Y * Z) ~> (X * Y) * Z
      def inverse[X]: Rassoc[X] ~> Lassoc[X] =
        type X1 = Fst[X]
        type X2 = Snd[X]
        type X3 = Third[X]

        val x: Rassoc[X] ~> X1 = fst[X1, X2 * X3]
        val y: Rassoc[X] ~> X2 = snd[X1, X2 * X3] >>> fst[X2, X3]
        val z: Rassoc[X] ~> X3 = snd[X1, X2 * X3] >>> snd[X2, X3]

        val to: Rassoc[X] ~> Lassoc[X] = (x * y) * z
        to

      // prove that:
      // (from >>> to) <-> id[(A1 * A2) * A3]

    }

  def leftUnitor = ???
  def rightUnitor = ???

}
