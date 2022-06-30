package categories.simple.monoidalCategories.instances

import categories.simple.monoidalCategories.*
import categories.simple.{_, given}
import annotations1.*
import categories.simple.categoryExamples.{_, given}
import categories.simple.functor.Functor.Id
import categories.simple.functor.*
import isEqual.*

// This is an arrow in the Product Category
type **>[A, B] =
  ( _1[A] => _1[B],
    _2[A] => _2[B] )

// -----------------
// Product functor:
// -----------------
// It's only defined in tuples, where it behaves as the identity
type Tuple[T] = (_1[T], _2[T])
// In morphisms:
// (TA **> TB)  =>  (Product[TA] => Product[TB])

object product extends (Scala × Scala --> Scala) [Tuple]:
  def map[A, B](f : A **> B): Tuple[A] => Tuple[B] =
    case (a1, a2) => (f._1(a1), f._2(a2))

object ScalaTuplesAreMonoidal extends MonoidalCategory[Scala]:
  type ⨂[A, B] = (A, B)
  type I = EmptyTuple
  type Scala3[A, B] = Prod3[Scala, Scala, Scala][A, B]
  type ***>[A, B] = Prod3[Scala, Scala, Scala][A, B]

  def tensor: (Scala × Scala --> Scala) [Tuple] = product

  val associator: (Lassoc <===> Rassoc)[Scala3, Scala] =
    new NaturalIsomorphism with (Lassoc ==> Rassoc)[Scala3, Scala]:
      // (x ⨂ y) ⨂ z =:= ((x,y),z)
      val source: (Scala3 --> Scala)[Lassoc] =
        new Functor:
          def map[A, B](f: Scala3[A, B]): Lassoc[A] => Lassoc[B] =
            case ((x, y), z) =>
              val (f1, f2, f3) = f
              ( ( f1(x), f2(y) ), f3(z) )
      // x ⨂ (y ⨂ z) =:= (x,(y,z))
      val target: (Scala3 --> Scala)[Rassoc] =
        new Functor:
          def map[A, B](f: Scala3[A, B]): Rassoc[A] => Rassoc[B] =
            case (x, (y, z)) =>
              val (f1, f2, f3) = f
              ( f1(x), ( f2(y), f3(z) ) )
          // identities
          def prove_identities[X] = {

            val Scala3 = summon[Category[Scala3]]

            // LHS:
            map(Scala3.id[X]) <-> Scala.id[Rassoc[X]]
                                  Scala.id[Rassoc[X]] == Scala.id[(_1[X], (_2[X], _3[X]))]

                 Scala3.id[X] == (Scala.id[_1[X]], Scala.id[_2[X]], Scala.id[_3[X]])
            // thus
            map(Scala3.id[X]) <-> Scala.id[(_1[X], (_2[X], _3[X]))]
                                  Scala.id[(_1[X], (_2[X], _3[X]))] <-> identity[(_1[X], (_2[X], _3[X]))]

            map(Scala.id[_1[X]], Scala.id[_2[X]], Scala.id[_3[X]]) <-> identity[(_1[X], (_2[X], _3[X]))]

          }

      def apply[A]   = { case ((x, y), z) => (x, (y, z)) }
      def inverse[A] = { case (x, (y, z)) => ((x, y), z) }

  val leftUnitor : (L <===> Id)[Scala, Scala] =
    new NaturalIsomorphism with (L ==> Id)[Scala, Scala]:
      // L[X] =  I ⨂ X
      val source: (Scala --> Scala)[L] =
        new Functor:
          def map[A, B](f: A => B): L[A] => L[B] =
            { case (i, a) => (i, f(a)) }

      val target: (Scala --> Scala)[Id] = Functor.identity[Scala]
      def apply[A] = { case (_, a) => a }
      def inverse[A] = a => (EmptyTuple, a)

  val rightUnitor: (R <===> Id)[Scala, Scala] =
    new NaturalIsomorphism with (R ==> Id)[Scala, Scala]:
      // R[X] =  X ⨂ I
      val source: (Scala --> Scala)[R] =
        new Functor:
          def map[A, B](f: A => B) = { case (a, i) => (f(a), i) }
      val target: (Scala --> Scala)[Id] = Functor.identity[Scala]
      def apply[A] = _._1
      def inverse[A] = a => (a, EmptyTuple)

