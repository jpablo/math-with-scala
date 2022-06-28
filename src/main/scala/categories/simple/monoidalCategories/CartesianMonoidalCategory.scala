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
  type Prod[T] = _1[T] * _2[T]

  type I = T

  // Arrows in the product categories
  type C2[A, B] = Prod2[C, C   ][A, B]
  type C3[A, B] = Prod3[C, C, C][A, B]
  // =:=
  //  ( _1[A] ~> _1[B],
  //    _2[A] ~> _2[B] )

  val tensor: Bifunctor[⨂, C2, C] =
    new Functor:
      def map[A, B](f : C2[A, B]): C[Prod[A], Prod[B]] =
        val (f1, f2) = f
        ???
//        f2 ** f2


  // This follows
  val associator: (Lassoc <===> Rassoc)[C3, C] =
    new NaturalIsomorphism with (Lassoc ==> Rassoc)[C3, C] {
      // (x ⨂ y) ⨂ z =:= (x * y) * z
      val source: (C3 --> C)[Lassoc] =
        new Functor:
          def map[A, B](f: C3[A, B]): C[ Lassoc[A], Lassoc[B] ] =
            val (f1, f2, f3) = f
            (f1 ** f2) ** f3

      // x ⨂ (y ⨂ z) =:= (x,(y,z))
      val target: (C3 --> C)[Rassoc] =
        new Functor:
          def map[A, B](f: C3[A, B]): C[ Rassoc[A], Rassoc[B] ] =
            val (f1, f2, f3) = f
            f1 ** (f2 ** f3)

      // (X * Y) * Z ~> X * (Y * Z)
      def apply[X]: Lassoc[X] ~> Rassoc[X] =
        type A1 = _1[X]
        type A2 = _2[X]
        type A3 = _3[X]

        val a: Lassoc[X] ~> A1 = fst[A1 * A2, A3] >>> fst[A1, A2]
        val b: Lassoc[X] ~> A2 = fst[A1 * A2, A3] >>> snd[A1, A2]
        val c: Lassoc[X] ~> A3 = snd[A1 * A2, A3]

        val from: Lassoc[X] ~> Rassoc[X] = a * (b * c)
        from

      // X * (Y * Z) ~> (X * Y) * Z
      def inverse[X]: Rassoc[X] ~> Lassoc[X] =
        type A1 = _1[X]
        type A2 = _2[X]
        type A3 = _3[X]

        val x: Rassoc[X] ~> A1 = fst[A1, A2 * A3]
        val y: Rassoc[X] ~> A2 = snd[A1, A2 * A3] >>> fst[A2, A3]
        val z: Rassoc[X] ~> A3 = snd[A1, A2 * A3] >>> snd[A2, A3]

        val to: Rassoc[X] ~> Lassoc[X] = (x * y) * z
        to

      // prove that:
      // (from >>> to) <-> id[(A1 * A2) * A3]

    }

  def leftUnitor = ???
  def rightUnitor = ???

}
