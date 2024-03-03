package categories.simple.functor

import categories.simple.{*, given}
import categories.simple.categoryExamples.{*, given}
import categories.simple.categoryExamples.*

// Bifunctor
type Bifunctor[F[_, _], From[_, _], To[_, _]] =
  (From --> To)[ [A] =>> F[Fst[A],  Snd[A]] ]
//  Functor[[A] =>> F[Fst[A],  Snd[A]], Prod, D]

// alternative definition with explicit ×
trait Bifunctor2[F[_, _], C1[_, _], C2[_, _], D[_, _]](
  using Category[C1 × C2], Category[D]
) extends
  Functor[[A] =>> F[Fst[A],  Snd[A]], C1 × C2, D]
// type ~> = D
// def [A, B, Ap, Bp](a: A ~> B) ⨂ (b: Ap ~> Bp) : (A x Ap) ~> (B x Bp) =
//   map[(A, Ap), (B, Bp)]((a, b))

object Examples:
  // Arrows in Scala × Scala have this form:
  type ProductMorphism[A, B] =
    (Fst[A] => Fst[B], Snd[A] => Snd[B])

  // Intersection is a bifunctor
  type Intersection[A] = Fst[A] & Snd[A]

  object intersection extends (Scala × Scala --> Scala) [Intersection]:
    def map[A, B](f: ProductMorphism[A, B]): Intersection[A] => Intersection[B] = ???
//      (fa: Fst[A]) => f._1(fa)
    // it satisfies the functor laws

  // Not sure union is a bifunctor
  type Union[A] = Fst[A] | Snd[A]

  object union extends (Scala × Scala --> Scala) [Union]:
    def map[A, B](f: ProductMorphism[A, B]): Union[A] => Union[B] =
    {
      case fa: Fst[A] => f._1(fa)
      case sa: Snd[A] => f._2(sa)  // this is probably not correct!!
    }

  type Product[A] =
    (Fst[A], Snd[A])

  object product extends Functor[Product, Scala × Scala, Scala]:
    def map[A, B](f: ProductMorphism[A, B]): Product[A] => Product[B] =
      case (a1, a2) => (f._1(a1), f._2(a2))


