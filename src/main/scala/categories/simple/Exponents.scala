package categories.simple

import discipline1.*
import annotations1.*

trait Exponents[~>[_, _]](Cat: Category[~>], val P: InternalProduct[~>]):
  import P.{`*`, `**`}
  import Cat.{◦, id}
  // A Category has exponentiation if it has a products
  // and for objects A, B there is an object B^A
  type ^[B, A]

  // and arrow
  def ev[A, B]: ((B^A) * A) ~> B
  // called evaluation arrow

  // such that
  def unique[A, B, C](g: (C * A) ~> B): C ~> (B^A)

  @Law
  def property[A, B, C](g: (C * A) ~> B): IsEq[(C * A) ~> B] =
    ev ◦ (unique(g) ** id[A]) <-> g

