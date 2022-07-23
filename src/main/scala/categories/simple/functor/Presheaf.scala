package categories.simple.functor

import categories.simple.{*, given}
import categories.simple.functor.*


/**
 * A general presheaf of D on C is a functor F from the opposite category Op[C] to F
 */
type Presheaf0[C[_, _], D[_, _]] =
  [F[_]] =>> (Op[C] --> D)[F]

// we're interested mostly on:
type Presheaf[C[_, _]] =
  [F[_]] =>> (Op[C] --> Scala)[F]

// i.e.
// Presheaf[C][F] =:= Functor[F, Op[C], Scala]

// A functor C[_, X0]
type HomFuncOp[C[_, _], X0] =
  [A] =>> C[A, X0]

//def presheaf[C[_, _]: Category, X0]: Presheaf[C][HomFuncOp[C, X0]] =
//  homFunctor[Op[C], X0](using Op[C])

def presheaf[C[_, _]: Category, X0]: (Op[C] --> Scala)[C[*, X0]] =
  homFunctor[Op[C], X0]


object Example:
  trait C[A, B]
  trait X0
  type T = Presheaf[C][HomFuncOp[C, X0]]
  summon[ T =:= (Op[C] --> Scala) [ HomFuncOp[C, X0]    ] ]
  summon[ T =:= (Op[C] --> Scala) [ HomFunc[Op[C], X0]  ] ]
  summon[ T =:= (Op[C] --> Scala) [ [A] =>> Op[C][X0, A]] ]
  summon[ T =:= (Op[C] --> Scala) [ [A] =>> C[A, X0]    ] ]
