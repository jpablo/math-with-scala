package categories.simple.monoidalCategories.props

import categories.simple.CategoryS
import categories.simple.monoidalCategories.StrictMonoidal
import categories.simple.functor.*

trait Prop[C[_ <: Int, _ <: Int]] extends CategoryS[Int, C]:
  type I
  type ⨂[_, _]
//  def tensor: Bifunctor[⨂, C × C, C]
