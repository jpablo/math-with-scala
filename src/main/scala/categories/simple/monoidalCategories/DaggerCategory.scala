package categories.simple.monoidalCategories

import annotations1.*
import categories.simple.*
import categories.simple.categoryExamples.*
import categories.simple.functor.Functor.Id
import categories.simple.functor.*
import isEqual.*

trait DaggerCategory[C[_, _]] extends Category[C]:
  extension [X, Y] (f: X ~> Y)
    def dag: Y ~> X

  @Law
  def daggerEquation1[X, Y, Z](f: X ~> Y, g: Y ~> Z) =
    (g ◦ f).dag <-> f.dag ◦ g.dag

  @Law
  def daggerEquation2[X, Y](f: X ~> Y) =
    f.dag.dag <-> f

