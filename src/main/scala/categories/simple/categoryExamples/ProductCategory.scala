package categories.simple.categoryExamples

import categories.simple.{_, given}


// tuple extractors

/**
 * _1[(A, B)] == A
 * @tparam A
 */
type _1[A] = A match { case Tuple => Tuple.Elem[A, 0] }
type _2[A] = A match { case Tuple => Tuple.Elem[A, 1] }
type _3[A] = A match { case Tuple => Tuple.Elem[A, 2] }

// type _1[X] = X match { case a *: _            => a }
// type _2[X] = X match { case _ *: b *: _       => b }
// type _3[X] = X match { case _ *: _ *: c  *: _ => c }


// ----------------------------
// Product category:
// Morphisms are pairs of arrows
// Objects are pairs of objects
// ----------------------------

type Prod2[~>[_, _], ->[_, _]] = [A, B] =>>
  ( _1[A] ~> _1[B],
    _2[A] -> _2[B] )

// infix alias
type ×[C[_, _], D[_, _]] =
  [A, B] =>> Prod2[C, D][A, B]



// Scala × Scala = [A, B] =>> (_1[A] => _1[B], _2[A] => _2[B])
// Scala2[A, B] =:= (Scala × Scala)[A, B]
// Scala2[(A[1], A[2]), (B[1], B[2])] = (A[1] => B[1], A[2] => B[2])
type Scala2[A, B] =
  ( _1[A] => _1[B],
    _2[A] => _2[B] )

// Prod3[C, C, C] is the category with objects Tuple3[_,_,_]
// and morphisms
// [A, B] =>> (_1[A] ~> _1[B], _2[A] ~> _2[B], _3[A] ~> _3[B])

// General product of three categories
type Prod3[~>[_, _], ->[_, _], >>[_, _]] = [A, B] =>>
  ( _1[A] ~> _1[B],
    _2[A] -> _2[B],
    _3[A] >> _3[B] )


trait Ex1:

  type C[_, _]; type A; type B

  summon[ (((C × C) × C)[A, B]) =:= ((C × C × C)[A, B]) ]

  summon[
    (C × C × C)[A, B] =:=
    ( (C[_1[_1[A]],  _1[_1[B]]], C[_2[_1[A]], _2[_1[B]]]), C[_2[A], _2[B]] )
  ]

  // TODO: Doesn't compile in Scala 3.0.1
  // summon[
  //   (Scala × Scala × Scala)[A, B] =:=
  //   ( (_1[_2[A]] => _1[_2[B]], _2[_2[A]] => _2[_2[B]]), _1[A] => _1[B] )
  // ]

//  type Triple[X1, Y1, Z1, X2, Y2, Z2] = ( (Y1 => Y2, Z1 => Z2), X1 => X2 )

  type X; type Y; type Z
  // A = (X, (Y, Z))
  summon[ _1[_2[(X, (Y, Z))]] =:= Y  ]

// -------------------------
// Product of two categories
// C × D
// -------------------------
extension
  [C[_, _], D[_, _]]
  (C: Category[C]) def × (D: Category[D])
: Category[C × D] =
  import C.◦ as compose1
  import D.◦ as compose2

  new Category[C × D]:
    def id[A]: A ~> A = ( C.id[_1[A]], D.id[_2[A]] )
    extension [A, B, C]
      (g: B ~> C) def ◦ (f: A ~> B): A ~> C =
      ( g._1 compose1 f._1, g._2 compose2 f._2 )

end extension

def prod3[C[_, _], D[_, _], E[_, _]](C: Category[C], D: Category[D], E: Category[E]): Category[Prod3[C, D, E]] =
  import C.{◦ => *}
  import D.{◦ => +}
  import E.{◦ => x}

  new Category[Prod3[C, D, E]]:
    def id[A]: A ~> A =
      (C.id[_1[A]], D.id[_2[A]], E.id[_3[A]])

    extension [A, B, C] (g: B ~> C) def ◦ (f: A ~> B): A ~> C =
      (g._1 * f._1, g._2 + f._2, g._3 x f._3)

given prodCat[C[_, _], D[_, _]](using C: Category[C], D: Category[D]): Category[C × D] =
  C × D

given prodCat3[C[_, _]](using C: Category[C]): Category[Prod3[C, C, C]] =
  prod3(C, C, C)

object Example:
  summon[Category[Scala × Scala]]
  summon[Category[Prod3[Scala, Scala, Scala]]]

