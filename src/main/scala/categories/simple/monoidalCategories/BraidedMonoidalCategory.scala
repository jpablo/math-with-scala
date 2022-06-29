package categories.simple.monoidalCategories

import categories.simple.*
import annotations1.*
import categories.simple.categoryExamples.*
import categories.simple.functor.Functor.Id
import categories.simple.functor.*
import discipline1.*

trait BraidedMonoidalCategory[C[_, _]](
  using 
    val C: Category[C],
    val M: MonoidalCategory[C]
):
  import C.*; export C.*
  import M.*; export M.{⨂, I}

  type Tensor[A]   = (_1[A] ⨂ _2[A])
  type Braiding[A] = (_2[A] ⨂ _1[A])

  // ∀ x,y: x ⨂ y <===>  y ⨂ x
  val braiding: (Tensor <===> Braiding)[C, C]
  def b[X, Y]  : (X ⨂ Y) ~> (Y ⨂ X) = braiding.iso[(X, Y)].from
  def bI[X, Y] : (X ⨂ Y) <~ (Y ⨂ X) = braiding.iso[(X, Y)].to

  @Law
  def hexagonEquation1[X, Y, Z] =
    (aI[X, Y, Z] >>> (b[X, Y] ⨂ id[Z]) >>> a[Y, X, Z] >>> (id[Y] ⨂ b[X, Z]) >>> aI[Y, Z, X]) <->
      b[X, Y ⨂ Z]

  @Law
  def hexagonEquation2[X, Y, Z] =
    (a[X, Y, Z] >>> (id[X] ⨂ b[Y, Z]) >>> aI[X, Z, Y] >>> (b[X, Z] ⨂ id[Y]) >>> a[Z, X, Y]) <->
      b[X ⨂ Y, Z]
