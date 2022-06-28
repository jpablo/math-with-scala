package categories.simple

import annotations1.*

trait Grupoid[~>[_, _]] extends Category[~>]:
  // A Grupoid is a category where every arrow has an inverse:
  extension [X, Y] (f: X ~> Y) def inv: Y ~> X

  // satisfying
  @Law
  def iso[X, Y](f: X ~> Y) =
    Isomorphism(f, f.inv)(using this)


