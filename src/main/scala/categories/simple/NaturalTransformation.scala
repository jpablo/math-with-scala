package categories.simple

import categories.simple.categoryExamples.*
import categories.simple.functor.*
import isEqual.*
import annotations1.*

/**
 * A Natural transformation is basically a polymorphic function
 * between the images of two functors F[_] and G[_]:
 *
 *  <pre>
 *      φ: [A] => F[A] => G[A]
 *  </pre>
 * with some conditions.
 *
 * This means that for each type A we have an arrow φ[A]: F[A] ~> G[A]
 * <pre>
 *
 *            φ[X]
 *     F[X]    ~~>   G[X]
 *       |             |
 *  F(f) |             | G(x)
 *       V             V
 *     F[Y]   ~~>    G[Y]
 *            φ[Y]
 * </pre>
 */
trait NaturalTransformation[
  Source[_],
  Target[_],
  C[_, _] : Category,
  D[_, _] : Category
] { self =>
    type ~>[A, B] = D[A, B]
    type ==>[F[_], G[_]] = NaturalTransformation[F, G, C, D]

    // Two functors from C to D
    val source: (C --> D) [Source]
    val target: (C --> D) [Target]

    // For each object X in C, an arrow in D
    def apply[X]: Source[X] ~> Target[X]

    /**
     * vertical composition:  ψ * φ: H ==> G
     */
    def * [H[_]] (other: H ==> Source): H ==> Target =
      NaturalTransformation(other.source, self.target, [X] => () => self[X] ◦ other[X])

    @Law
    def naturality[X, Y](
      source: (C --> D) [Source],
      target: (C --> D) [Target],
      φ     : Source ==> Target,
      h     : C[X, Y]
    ) =
      φ[Y] ◦ source(h) <-> target(h) ◦ φ[X]
}

/**
 * A Natural Transformation between the functors F and G.
 */
type ==>[F[_], G[_]] =
  [C[_, _], D[_, _]] =>> NaturalTransformation[F, G, C, D]


object NaturalTransformation:
  // Helper constructor
  def apply
    [F[_], G[_], C[_, _]: Category, D[_, _]: Category]
    (
      source : (C --> D)[F],
      target : (C --> D)[G],
      nat    : [X] => () => D[F[X], G[X]],
    )
  : (F ==> G)[C, D] =
    val (s, t) = (source, target)
    new NaturalTransformation:
      val source = s
      val target = t
      def apply[X] = nat[X]()

  def identity
    [F[_], C[_, _]: Category, D[_, _]: Category]
    (f: (C --> D)[F])
  : (F ==> F)[C, D] =
      NaturalTransformation(f, f, [X] => () => Category[D].id[F[X]])

end NaturalTransformation


/**
 * Whisker composition:
 *
 * whisker(k, nat, h): K ⊙ F ⊙ H ==> K ⊙ G ⊙ H
 */
def whisker
  [
    H[_], F[_], G[_], K[_],
    Cp[_, _]: Category, C[_, _]: Category,
    D[_, _]: Category, Dp[_, _]: Category
  ]
  (
    k   : (D  --> Dp) [K],
    nat : (F  ==> G)  [C, D],
    h   : (Cp --> C)  [H],
  )
: (K ⊙ F ⊙ H ==> K ⊙ G ⊙ H)[Cp, Dp] =
  NaturalTransformation(
    source = k ⊙ nat.source ⊙ h,
    target = k ⊙ nat.target ⊙ h,
    nat    = [X] => () => k( nat[H[X]] )
  )


//========================================================================================
/*                                                                                      *
 * mixed composition                                                                    *
 * (φ *: H): F ⊙ H ==> G ⊙ H                                                            *
 *                                                                                      */
//========================================================================================

extension [H[_], F[_], G[_], Cp[_, _]: Category, C[_, _]: Category, D[_, _]: Category] (nat : (F ==> G)[C, D])
  def *: (h : (Cp --> C)[H]): (F ⊙ H ==> G ⊙ H) [Cp, D] =
    whisker(Functor.identity[D], nat, h)

//========================================================================================
/*                                                                                      *
 * (K :* φ): K ⊙ F ==> K ⊙ G                                                            *
 *                                                                                      */
//========================================================================================
extension [F[_], G[_], K[_], C[_, _]: Category, D[_, _]: Category, Dp[_, _]: Category] (k: (D --> Dp)[K])
  def :* (nat: (F ==> G)[C, D]): (K ⊙ F ==> K ⊙ G) [C, Dp] =
    whisker(k, nat, Functor.identity[C])


trait NaturalIsomorphism
  [F[_], G[_], C[_,_], D[_, _]]
  (using
    C: Category[C],
    D: Category[D]
  )
extends NaturalTransformation[F, G, C, D]:
  import D.~>

  // A family of arrows
  def inverse[A]: G[A] ~> F[A]

  // A family of isomorphisms
  def iso[A] =
    new Isomorphism(apply[A]: F[A] ~> G[A], inverse[A])

end NaturalIsomorphism

// F <==> G
type <===>[F[_], G[_]] =
  [C[_, _], D[_, _]] =>> NaturalIsomorphism[F, G, C, D]
