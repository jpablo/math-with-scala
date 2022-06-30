package categories.simple.functor

import categories.simple.*
import categories.simple.functor.*
import categories.simple.monoidalCategories.MonoidalCategory
import categories.simple.categoryExamples.*

/**
 * A lax monoidal functor F from categories C to D
 * @tparam F
 * @tparam C A monoidal category
 * @tparam D A monoidal category
 */
trait MonoidalFunctor
  [F[_], C[_, _], D[_, _]]
  (using
    val C: MonoidalCategory[C],
    val D: MonoidalCategory[D]
  ) extends Functor[F, C, D]:
  import D.⨂ as ●; import C.⨂

  type `F[A] ● F[B]`[T] = F[Fst[T]] ● F[Snd[T]]
  type `F[A ⨂ B]`[T] = F[Fst[T] ⨂ Snd[T]]

//  private val f1: (C × C --> D)[F1] = ???
//  private val f2: (C × C --> D)[F2] = ???

  // ------------------------------------
  // Coherence maps or Structure morphisms:
  // ------------------------------------

  /**
   * A natural transformation {{{ [A × B] =>> F[A] ● F[B] ~> F[A ⨂ B] }}}
   */
  val nat: (`F[A] ● F[B]` ==> `F[A ⨂ B]`)[C × C, D]
  /**
   * A morphism:
   */
  val e: D.I ~> F[C.I]

end MonoidalFunctor
