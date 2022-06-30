package categories.simple.monoidalCategories

import annotations1.*
import categories.simple.*
import categories.simple.categoryExamples.*
import categories.simple.functor.Functor.Id
import categories.simple.functor.*
import isEqual.*

trait SymmetricMonoidalCategory[C[_, _]] extends BraidedMonoidalCategory[C]:
  
  @Law
  def symmetry[X, Y] = b[X, Y] <-> bI[Y, X]
