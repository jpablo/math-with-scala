package algebra

import annotations1.*
import isEqual.*

trait VectorSpace[V, F](
  using
  V: AbelianGroup[V],
  F: Field[F]
):
  extension (a: V) def + (b: V): V = a * b
  extension (a: F) def ⋅ (b: V): V

  @Law("Compatibility of scalar multiplication with field multiplication")
  def compatibility(a: F, b: F, v: V) =
    a ⋅ (b ⋅ v) <-> (a * b) ⋅ v

  @Law("Identity element of scalar multiplication")
  def scalarMulIdentity(v: V) =
    F.one ⋅ v <-> v

  @Law("Distributivity of scalar multiplication over vector addition")
  def distributivity1(a: F, u: V, v: V) =
    a ⋅ (u + v) <-> (a ⋅ u) + (a ⋅ v)

  @Law("Distributivity of scalar multiplication over field addition")
  def distributivity2(a: F, b: F, v: V) =
    (a + b) ⋅ v <-> (a ⋅ v) + (b ⋅ v)
