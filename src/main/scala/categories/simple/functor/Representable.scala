package categories.simple.functor

import categories.simple.{*, given}
import categories.simple.functor.*

/**
 * A functor F: C --> Scala is said to be representable
 * if it's isomorphic to a hom functor:
 *    F <-> HomFunctor[C, X0]
 * for some X0 in C
 * X0 is said to represent F
 */
trait Representable[F[_], C[_, _]: Category] extends Functor[F, C, Scala]:
  type X0
  val iso: (F <===> HomFunc[C, X0]) [C, Scala]

