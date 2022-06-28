package categories.simple.monoidalCategories

import annotations1.*
import categories.simple.*
import categories.simple.categoryExamples.*
import categories.simple.functor.Functor.Id
import categories.simple.functor.*
import discipline1.*

trait ClosedMonoidalCategory[C[_, _]] extends MonoidalCategory[C]:
  type ~~>[_, _]
  val internalHom: Bifunctor[~~>, Op[C] × C, C]
  // currying (right closed)
  type LC[A] = A match { case (x, y, z) => C[x ⨂ y, z] }
  type RC[A] = A match { case (x, y, z) => C[y, x ~~> z] }

  val currying: (LC <===> RC)[C, C]
  def c[X, Y, Z] : C[X ⨂ Y, Z] ~> C[Y, X ~~> Z] = currying.iso[(X, Y, Z)].from

