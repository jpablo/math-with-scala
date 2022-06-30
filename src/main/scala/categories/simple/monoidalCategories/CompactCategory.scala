package categories.simple.monoidalCategories

import annotations1.*
import categories.simple.*
import categories.simple.categoryExamples.*
import categories.simple.functor.Functor.Id
import categories.simple.functor.*
import isEqual.*

trait CompactCategory[C[_, _]] extends MonoidalCategory[C]:
  type Dual[X]
  // unit
  def i[X]: I ~> (Dual[X] ⨂ X)
  // counit
  def e[X]: (X ⨂ Dual[X]) ~> I

  @Law
  def zigZagEquation1[X] =
    (id[X] ⨂ i[X]) >>> aI[X, Dual[X], X] >>> (e[X] ⨂ id[X]) >>> l[X] <-> r[X]

  @Law
  def zigZagEquation2[X] = ???
//    (i[X] ⨂ id[Dual[X]]) >>> a[Dual[X], X, Dual[X]] >>> (id[Dual[X]] ⨂ e[X]) >>> r[Dual[X]] <-> l[X]

