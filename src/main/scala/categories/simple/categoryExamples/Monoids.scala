package categories.simple.categoryExamples

import categories.simple.CategoryS
import zio.prelude.*

// Monoids are categories with only one object
// trick: each element is an arrow

type ● = "Singleton"

type ConstantHom[A] = [_ <: ●, _ <: ●] =>> A

/**
 * Creates a new Category given a Monoid[M]
 */
def fromMonoid[M](M: classic.Monoid[M]) =
  new CategoryS[●, ConstantHom[M]]:
    def id[A <: ●]: M = M.identity
    extension [A <: ●, B <: ●, C <: ●] 
      (g: M) def ◦ (f: M) = M.combine(g, f)

  
object IntCategory extends CategoryS[●,  ConstantHom[Int]]:
  def id[A <: ●] = 0
  extension [A <: ●, B <: ●, C <: ●] (g: Int) def ◦ (f: Int) =
    g + f

object Examples:
  given StringMonoidCat: CategoryS[●, ConstantHom[String]] =
    fromMonoid(Identity[String])

  assert("a" ◦ "b" == "ab")

  import IntCategory.◦
  
  assert(1 ◦ 2 == 3)
