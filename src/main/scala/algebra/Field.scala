package algebra

import annotations1.*
import discipline1.*
// import scala.language.strictEquality

trait Field[F](
    Sum : AbelianGroup[F],
    Mult: AbelianGroup[F] // \ {0}
  )(using CanEqual[F, F]):

  extension (a: F)
    def + (b: F): F = Sum.* (a) (b)

    def * (b: F): F = Mult.* (a) (b)

    // additive inverse
    def invS: F = Sum.inv(a)

    // multiplicative inverse
    def invM: Option[F] = if a != _0 then Some(Mult.inv(a)) else None

  def _0 = Sum.id

  def _1 = Mult.id

  @Law
  def distributivity (a: F, b: F, c: F): IsEq[F] =
    a * (b + c) <-> (a * b) + (a * c)

end Field
