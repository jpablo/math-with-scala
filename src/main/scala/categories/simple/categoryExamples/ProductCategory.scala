package categories.simple.categoryExamples

import categories.simple.{*, given}

import scala.annotation.targetName


// tuple extractors

/**
 * Fst[(A, B)] == A
 */
type Fst[A]   = A match { case Tuple => Tuple.Elem[A, 0] }
type Snd[A]   = A match { case Tuple => Tuple.Elem[A, 1] }
type Third[A] = A match { case Tuple => Tuple.Elem[A, 2] }

// type Fst[X]   = X match { case a *: _            => a }
// type Snd[X]   = X match { case _ *: b *: _       => b }
// type Third[X] = X match { case _ *: _ *: c  *: _ => c }


// ----------------------------
// Product category:
// Morphisms are pairs of arrows
// Objects are pairs of objects
// ----------------------------

type Prod2[~>[_, _], ->[_, _]] = [A, B] =>>
  ( Fst[A] ~> Fst[B],
    Snd[A] -> Snd[B] )

// infix alias
type ×[C[_, _], D[_, _]] =
  [A, B] =>> Prod2[C, D][A, B]



// Scala × Scala = [A, B] =>> (Fst[A] => Fst[B], Snd[A] => Snd[B])
// Scala2[A, B] =:= (Scala × Scala)[A, B]
// Scala2[(A[1], A[2]), (B[1], B[2])] = (A[1] => B[1], A[2] => B[2])
type Scala2[A, B] =
  ( Fst[A] => Fst[B],
    Snd[A] => Snd[B] )

// Prod3[C, C, C] is the category with objects Tuple3[_,_,_]
// and morphisms
// [A, B] =>> (Fst[A] ~> Fst[B], Snd[A] ~> Snd[B], Third[A] ~> Third[B])

// General product of three categories
type Prod3[~>[_, _], ->[_, _], >>[_, _]] = [A, B] =>>
  ( Fst[A]   ~> Fst[B],
    Snd[A]   -> Snd[B],
    Third[A] >> Third[B] )


trait Ex1:

  type C[_, _]; type A; type B

  summon[ (C × C × C)[A, B] =:= ((C × C) × C)[A, B] ]

  summon[
    (C × C × C)[A, B] =:=
    (
      ( C[Fst[Fst[A]],  Fst[Fst[B]]], C[Snd[Fst[A]], Snd[Fst[B]]] ),
      C[Snd[A], Snd[B]]
    )
  ]

   summon[
     (Scala × Scala × Scala)[A, B] =:=
     (
       (Fst[Fst[A]] => Fst[Fst[B]], Snd[Fst[A]] => Snd[Fst[B]]),
       Snd[A] => Snd[B]
     )
   ]


  type X; type Y; type Z
  // sanity check
  summon[ Fst[Snd[(X, (Y, Z))]] =:= Y  ]

end Ex1

// -------------------------
// Product of two categories
// C × D
// -------------------------
extension [C[_, _], D[_, _]] (C: Category[C])
  def × (D: Category[D]): Category[C × D] =

    import C.◦ as compose1
    import D.◦ as compose2

    new Category[C × D]:
      def id[A]: A ~> A = ( C.id[Fst[A]], D.id[Snd[A]] )
      extension [A, B, C] (g: B ~> C)
        @targetName("compose")
        def ◦ (f: A ~> B): A ~> C =
          ( g._1 `compose1` f._1, g._2 `compose2` f._2 )

end extension

def prod3[C[_, _], D[_, _], E[_, _]](C: Category[C], D: Category[D], E: Category[E]): Category[Prod3[C, D, E]] =
  import C.◦ as *
  import D.◦ as +
  import E.◦ as x

  new Category[Prod3[C, D, E]]:
    def id[A]: A ~> A =
      (C.id[Fst[A]], D.id[Snd[A]], E.id[Third[A]])

    extension [A, B, C] (g: B ~> C)
      @targetName("compose")
      def ◦ (f: A ~> B): A ~> C =
        (g._1 * f._1, g._2 + f._2, g._3 `x` f._3)

given prodCat[C[_, _], D[_, _]](using C: Category[C], D: Category[D]): Category[C × D] =
  C × D

given prodCat3[C[_, _]](using C: Category[C]): Category[Prod3[C, C, C]] =
  prod3(C, C, C)

object Example:
  summon[Category[Scala × Scala]]
  summon[Category[Prod3[Scala, Scala, Scala]]]

