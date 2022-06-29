package categories.simple.monoidalCategories

import categories.simple.{*, given}
import annotations1.*
import categories.simple.categoryExamples.{*, given}
import categories.simple.functor.Functor.Id
import categories.simple.functor.*
import discipline1.*

trait MonoidalCategory[C[_, _]](using C: Category[C]):
  export C.*
  type I
  type ⨂[_, _]
  def tensor:  Bifunctor[⨂, C × C, C]

  // Lassoc, Rassoc are a functors: (C, C, C) --> C
  // (A ⨂ B) ⨂ C
  type Lassoc[A] = (_1[A] ⨂ _2[A]) ⨂ _3[A]
  // A ⨂ (B ⨂ C)
  type Rassoc[A] = _1[A] ⨂ (_2[A] ⨂ _3[A])
  //  summon[ Lassoc[((Int, String), Long)] =:= ((Int ⨂ String) ⨂ Long)]
  //  summon[ Rassoc[((Int, String), Long)] =:=  (Int ⨂ (String ⨂ Long))]

  // Natural isomorphism ∀ x,y,z: (x ⨂ y) ⨂ z <===>  x ⨂ (y ⨂ z)
  def associator: (Lassoc <===> Rassoc)[Prod3[C, C, C], C]
  def a[X, Y, Z] : ((X ⨂ Y) ⨂ Z) ~> (X ⨂ (Y ⨂ Z)) = associator.iso[(X, Y, Z)].from
  def aI[X, Y, Z]: (X ⨂ (Y ⨂ Z)) ~> ((X ⨂ Y) ⨂ Z) = associator.iso[(X, Y, Z)].to

  // ∀ X: I ⨂ X <===> X
  type L[X] =  I ⨂ X
  def leftUnitor : (L <===> Id)[C, C]
  def l[X]: I ⨂ X ~> X = leftUnitor.iso[X].from

  // ∀X: X ⨂ I <===> X
  type R[X] =  X ⨂ I
  def rightUnitor: (R <===> Id)[C, C]
  def r[X] : X ⨂ I ~> X = rightUnitor.iso[X].from
  def rI[X]: X ⨂ I <~ X = rightUnitor.iso[X].to

  // this is tensor.map in infix form: f ⨂ g:
  // (since arrows in the product category are pairs of arrows)
  extension [A1, A2, B1, B2](f: A1 ~> A2) def ⨂ (g: B1 ~> B2): (A1 ⨂ B1) ~> (A2 ⨂ B2) =
    tensor[(A1, B1), (A2, B2)]((f, g))

  @Law
  def triangleEquation[X, Y] = //: IsEq[((X ⨂ I) ⨂ Y) ~> (X ⨂ Y)] =
    (a[X, I, Y] >>> (id[X] ⨂ l[Y])) <-> (r[X] ⨂ id[Y])

  @Law
  def pentagonEquation[W, X, Y, Z] =
    (a[W, X, Y] ⨂ id[Z]) >>> a[W, X ⨂ Y, Z] >>> (id[W] ⨂ a[X, Y, Z]) <-> (a[W ⨂ X, Y, Z] >>> a[W, X, Y ⨂ Z])

