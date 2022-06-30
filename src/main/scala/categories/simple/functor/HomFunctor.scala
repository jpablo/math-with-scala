package categories.simple.functor

import categories.simple.{_, given}
import categories.simple.functor.*


/**
 * Hom Functor: sends each object A into its hom-set Hom(X0, A)
 * @tparam C
 * @tparam X0
 */
type HomFunc[C[_, _], X0] =
  [A] =>> C[X0, A]

// and acts on arrows as composition by f.
def homFunctor[C[_, _]: Category, X0]: (C --> Scala)[ C[X0, _] ] =
  Functor {
    [A, B] => (f: C[A, B]) =>
      (g: C[X0, A]) => f ◦ g
  }
