package algebra

import annotations1.*
import discipline1.*

trait Field[F](
    Sum : AbelianGroup[F],
    Mult: AbelianGroup[F] // \ {0}
  ):

  extension (a: F)
    def + (b: F): F = Sum.* (a) (b)

    def * (b: F): F = Mult.* (a) (b)

    // additive inverse
    def invS: F = Sum.inv(a)

    // multiplicative inverse
    def invM: Option[F] =
      if a != zero then Some(Mult.inv(a)) else None

  def zero = Sum.id

  def one = Mult.id

  @Law
  def distributivity (a: F, b: F, c: F): IsEq[F] =
    a * (b + c) <-> (a * b) + (a * c)

end Field
