package categories.simple

import categories.simple.functor.*

trait UniversalProp
  [C[_, _], X, F[_]]
  (F: (C --> Scala)[F])
  (using Category[C]):
  val naturalIso: (C[X, _] <===> F)[C, Scala]


trait UniversalPropCo
  [C[_, _], X, P[_]]
  (P: (Op[C] --> Scala)[P])
  (using Category[Op[C]]):
  val naturalIso: (C[*, X] <===> P)[C, Scala]