package categories.simple.monoidalCategories

import categories.simple.{*, given}
import annotations1.*
import categories.simple.categoryExamples.{*, given}
import categories.simple.functor.*
import isEqual.*

import scala.<:<.refl


// A strict monoidal category is a category
// equipped with bifunctor, usually called a categorical tensor product
//  def tensor: (C × C --> C)[ [T] =>> _1[T] ⨂ _2[T] ]
//  def tensor: (C × C --> C)[ [(A, B)] =>> A ⨂ B ]

trait StrictMonoidal[C[_, _]] extends Category[C]:
  type I
  type ⨂[_, _]
  def tensor: Bifunctor[⨂, C × C, C]

  @Law
  def unitLeft[A]:
    (I ⨂ A) =:= A

  @Law
  def unitRight[A]:
    (A ⨂ I) =:= A

  @Law
  def associativity[A, B, C]:
    ((A ⨂ B) ⨂ C) =:= (A ⨂ (B ⨂ C))

//  import C.*; export C.*

  // Note: the tensor product induces a canonical binary operation in arrows.
  // This is just the Functor map applied to the arrows of the product category,
  // which are tuples of arrows.
  extension [A1, B1, A2, B2]
    ( f: A1 ~> B1) def ⨂ (
      g: A2 ~> B2): (A1 ⨂ A2) ~> (B1 ⨂ B2) =
    tensor[(A1, A2), (B1, B2)]((f, g))

  // Requiring that ⨂ is a bifunctor is the same as:
  @Law
  def tensorLaws[A1, A2, B1, B2, C1, C2](
    f1: A1 ~> B1, f2: A2 ~> B2,
    g1: B1 ~> C1, g2: B2 ~> C2
  ) =
    // F(f . g) = F(f). F(g)
    // F((f1,f2) . (g1, g2)) = F((f1,f2)) . F((g1, g2))
    // F((f1 . g1, f2 . g2)) = F((f1,f2)) . F((g1, g2))
    // (f1 . g1) ⨂ (f2 . g2) = (f1 ⨂ f2) . (g1 ⨂ g2)
    (g1 ◦ f1) ⨂ (g2 ◦ f2) <-> (g1 ⨂ g2) ◦ (f1 ⨂ f2)


object Example:

  type ProductArrow[A, B] =
    ( _1[A] => _1[B],
      _2[A] => _2[B] )

  type Scala2[A, B] = (Scala × Scala)[A, B]
  val Scala2 = Scala × Scala


  trait IntersectionIsMonoidal extends StrictMonoidal[Scala]:
    type I = Any
    type ⨂[A, B] = A & B
    type ∩[X] = _1[X] & _2[X]
    // i.e. ∩[(A, B)] =:= (A & B) =:= (A ⨂ B)
    // def example[X, Y] =
      // summon[∩[(X, Y)] =:= (X & Y)]

    def tensor: (Scala2 --> Scala)[∩] =
      new Functor:

        def map[A, B](f: ProductArrow[A, B]): ∩[A] => ∩[B] = ??? //f._1

        @Proof
        def compositionP[X, Y, Z](f: ProductArrow[X, Y], g: ProductArrow[Y, Z]) =
          import Scala2.{◦ => ◦◦}
          val (g1, g2) = g
          val (f1, f2) = f
          List(
                        map(g ◦◦ f) <-> map(g) ◦ map(f),
                        map(g ◦◦ f) <-> map((g1, g2)) ◦ map((f1, f2)),
                        map(g ◦◦ f) <-> g1 ◦ f1,
            map((g1 ◦ f1, g2 ◦ f2)) <-> g1 ◦ f1,
                            g1 ◦ f1 <-> g1 ◦ f1,
          )

        @Proof
        def identitiesP[X] =
          type X1 = _1[X]
          type X2 = _2[X]
          List(
                map(Scala2.id[X]) <-> id[∩[X]],
            map((id[X1], id[X2])) <-> id[∩[X]],
                         id[∩[X]] <-> id[∩[X]]
          )


    def unitLeft[A]: (Any & A) =:= A = refl
    def unitRight[A]: (A & Any) =:= A = refl
    def associativity[A, B, C]: (A & (B & C)) =:= ((A & B) & C) = refl

  trait UnionIsMonoidal_Possibly extends StrictMonoidal[Scala]:
    type I = Nothing
    type ⨂[A, B] = A | B
    type T[A] = _1[A] | _2[A]
    def tensor: (Scala × Scala --> Scala) [T] = categories.simple.functor.Examples.union
    def unitLeft[A]: (Nothing | A) =:= A = refl
    def unitRight[A]: (A | Nothing) =:= A = refl
    def associativity[A, B, C]: (A | (B | C)) =:= ((A | B) | C) = refl

