package categories.products

import categories.simple.*

import scala.annotation.targetName

// Motivation:
// instead of
// type Scala2[A, B] = ( Fst[A] => Fst[B], Snd[A] => Snd[B] )
// we'd like to write
// type Scala2[(A[1], A[2]), (B[1], B[2])] = (A[1] => B[1], A[2] => B[2])
// or
// type Scala2[(A1, A2), (B1, B2)] = (A1 => B1, A2 => B2)

// Option 1: type arguments can be 2-tuples
// Objects are 2-tuples (A, B)
// Arrows are pairs of arrows (f, g): (A1, A2) ~> (B1, B2) = (f: A1 ~> B1, g: A2 ~> B2)

// Find Hom = ~> such that
// (A1, A2) ~> (B1, B2) =:= (A1 => B1, A2 => B2)



trait CategoryP2[Hom[_, _]]:
  type ~>[A, B] = Hom[A, B]

  // An identity between 2-tuples
  def id[A1, A2]: (A1, A2) ~> (A1, A2)

  extension [A1, A2, B1, B2, C1, C2]
    (f: (A1, A2) ~> (B1, B2)) def >>> (g: (B1, B2) ~> (C1, C2)): (A1, A2) ~> (C1, C2)


object CategoryP2Examples extends App:

  // Example 1: Scala x Scala category

  // since we can't write [(A1,A2), (B1, B2)] =>> (A1 => B1, A2 => B2)
  type Scala2P[A, B] = (A, B) match
    case ((a1, a2), (b1, b2)) => (a1 => b1, a2 => b2)

  trait ScalaP2 extends CategoryP2[Scala2P] {

    def id[A1, A2]: (A1 => A1, A2 => A2) = (identity[A1], identity[A2])

    extension [A1, A2, B1, B2, C1, C2]
      (f: (A1 => B1, A2 => B2)) def >>> (g: (B1 => C1, B2 => C2)): (A1 => C1, A2 => C2) =
        (f._1 andThen g._1, f._2 andThen g._2)

  }

  // Example 2: Product of two arbitrary categories
  type Prod2P[~>[_, _], ->[_, _], A, B] = (A, B) match
    case ((a1, a2), (b1, b2)) => (a1 ~> b1, a2 -> b2)

  type ×[~>[_, _], ->[_, _]] =
    [A, B] =>> Prod2P[~>, ->, A, B]

  extension
    [C[_, _], D[_, _]]
    (C: Category[C]) def × (D: Category[D])
  : CategoryP2[C × D] =
    import C.>>> as andThen1
    import D.>>> as andThen2

    new CategoryP2[C × D]:
      def id[A1, A2] = (C.id[A1], D.id[A2])
      extension[A1, A2, B1, B2, C1, C2]
       (f: (A1, A2) ~> (B1, B2)) def >>>
       (g: (B1, B2) ~> (C1, C2)) =
          (f._1 `andThen1` g._1, f._2 `andThen2` g._2)
  end extension
end CategoryP2Examples


object CategoryTuples extends App:
  import categories.simple.categoryExamples.{Fst, Snd}
  // Example 2: Product of two arbitrary categories
  type Prod2P[~>[_, _], ->[_, _], A <: Tuple, B <: Tuple] =
    (Fst[A] ~> Fst[B], Snd[A] -> Snd[B])

  // Curried version
  type ×[~>[_, _], ->[_, _]] =
    [A <: Tuple, B <: Tuple] =>> Prod2P[~>, ->, A, B]

  type CategoryT[Hom[_ <: Tuple, _ <: Tuple]] = CategoryS[Tuple, Hom]

  extension [C[_, _], D[_, _]] (C: Category[C]) def × (D: Category[D]): CategoryT[C × D] =
    import C.◦ as *;import D.◦ as +
    new CategoryT[C × D]:
      def id[A <: Tuple]: A ~> A =
        (C.id[Fst[A]], D.id[Snd[A]])

      extension [X <: Tuple, Y <: Tuple, Z <: Tuple] (g: Y ~> Z)
        @targetName("compose")
        def ◦ (f: X ~> Y): X ~> Z =
          (g._1 * f._1, g._2 + f._2)


// Another option: Modeling tuples as (F[1], F[2], ..., F[n])

object CategoryP2FuncExamples extends App:
  import categories.highOrder.CategoryF1
  // Example 1: Scala2
  type Scala2F[A[_], B[_]] =
    (A[1] => B[1], A[2] => B[2])

  object ScalaP2 extends CategoryF1[Scala2F] {
    def id[A[_]]: (A[1] => A[1], A[2] => A[2]) = (identity[A[1]], identity[A[2]])
    extension [A[_], B[_], C[_]]
      (f: A ~> B) def andThen (g: B ~> C): A ~> C =
        (f._1 andThen g._1, f._2 andThen g._2)
  }

  // Example 2: Product of two arbitrary categories
  type Prod2F[~>[_, _], ->[_, _], A[_], B[_]] =
    (A[1] ~> B[1], A[2] -> B[2])

  // Curried version
  type ×[~>[_, _], ->[_, _]] =
    [A[_], B[_]] =>> Prod2F[~>, ->, A, B]

  extension
    [C[_, _], D[_, _]]
    (C: Category[C]) def × (D: Category[D])
  : CategoryF1[C × D] =
    import C.>>> as andThen1
    import D.>>> as andThen2

    new CategoryF1[C × D]:
      def id[A[_]] = (C.id[A[1]], D.id[A[2]])
        : A ~> A
      extension [A[_], B[_], C[_]]
        (f: A ~> B) def andThen (g: B ~> C) = (f._1 `andThen1` g._1, f._2 `andThen2` g._2)
        : A ~> C

end CategoryP2FuncExamples
